{-# LANGUAGE RecordWildCards #-}

module Heist.Extra.Splices.Pandoc.Ctx (
  RenderCtx (..),
  RenderFeatures (..),
  CodeBackend (..),
  MathBackend (..),
  defaultFeatures,
  mkRenderCtx,
  emptyRenderCtx,
  getUserData,
  setUserData,
  rewriteClass,
  ctxSansCustomSplicing,
  concatSpliceFunc,
) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Heist qualified as H
import Heist.Extra.Splices.Pandoc.Attr (concatAttr)
import Heist.Interpreted qualified as HI
import Text.Pandoc.Builder qualified as B
import Text.XmlHtml qualified as X

-- | Backend for syntax-highlighting fenced code blocks.
data CodeBackend
  = -- | Emit code blocks unhighlighted; consumer wires client-side JS if desired.
    NoHighlighting
  | -- | Tokenize at build time via @skylighting@.
    Skylighting
  deriving stock (Eq, Show)

-- | Backend for rendering `$...$` / `$$...$$` math.
data MathBackend
  = -- | Emit raw delimiters for client-side JS (KaTeX, MathJax) to pick up.
    NoStaticMath
  | -- | Convert to MathML at build time via @texmath@.
    StaticMathML
  deriving stock (Eq, Show)

{- | Which rendering features are active. Selecting a non-trivial backend
per axis is independently extensible — adding a third math backend does
not touch call sites for the code backend, and vice versa.
-}
data RenderFeatures = RenderFeatures
  { codeHighlighting :: CodeBackend
  , mathRendering :: MathBackend
  }
  deriving stock (Eq, Show)

-- | Everything off — the historical default before either feature existed.
defaultFeatures :: RenderFeatures
defaultFeatures = RenderFeatures NoHighlighting NoStaticMath

{- | The configuration context under which we must render a `Pandoc` document
 using the given Heist template.
-}
data RenderCtx = RenderCtx
  { -- The XML node which contains individual AST rendering definitions
    -- This corresponds to pandoc.tpl
    rootNode :: Maybe X.Node
  , -- Attributes for a given AST node.
    bAttr :: B.Block -> B.Attr
  , iAttr :: B.Inline -> B.Attr
  , -- Class attribute rewrite rules
    classMap :: Map Text Text
  , -- Custom render functions for AST nodes.
    blockSplice :: B.Block -> Maybe (HI.Splice Identity)
  , inlineSplice :: B.Inline -> Maybe (HI.Splice Identity)
  , renderFeatures :: RenderFeatures
  , userData :: Dynamic
  -- ^ Caller-defined per-render context. Lets a downstream library attach a
  -- typed value (e.g. an embed-ancestor stack for cycle detection) without
  -- requiring it to live on every renderer's signature. Default 'toDyn ()'.
  -- Use 'getUserData' / 'setUserData' for type-safe access.
  }

mkRenderCtx ::
  (Monad m) =>
  -- | How to replace classes in Div and Span nodes.
  Map Text Text ->
  -- | Custom handling of AST block nodes
  (RenderCtx -> B.Block -> Maybe (HI.Splice Identity)) ->
  -- | Custom handling of AST inline nodes
  (RenderCtx -> B.Inline -> Maybe (HI.Splice Identity)) ->
  -- | Rendering feature selection (code highlighting, static math, …).
  RenderFeatures ->
  H.HeistT Identity m RenderCtx
mkRenderCtx classMap bS iS features = do
  node <- H.getParamNode
  let ctx =
        RenderCtx
          (Just node)
          (blockLookupAttr node)
          (inlineLookupAttr node)
          classMap
          (bS ctx)
          (iS ctx)
          features
          (toDyn ())
   in pure ctx

emptyRenderCtx :: RenderCtx
emptyRenderCtx =
  RenderCtx Nothing (const B.nullAttr) (const B.nullAttr) mempty (const Nothing) (const Nothing) defaultFeatures (toDyn ())

{- | Read a typed value from the ctx's 'userData' slot.

Returns 'Nothing' when the slot was never set or holds a value of a different
type. Callers that depend on the slot existing should treat 'Nothing' as the
identity-of-this-type default (e.g. an empty stack), not as an error — heist
itself never inspects 'userData'.
-}
getUserData :: forall a. (Typeable a) => RenderCtx -> Maybe a
getUserData = fromDynamic . userData

-- | Replace the ctx's 'userData' slot with a new typed value.
setUserData :: forall a. (Typeable a) => a -> RenderCtx -> RenderCtx
setUserData x ctx = ctx {userData = toDyn x}

-- | Strip any custom splicing out of the given render context
ctxSansCustomSplicing :: RenderCtx -> RenderCtx
ctxSansCustomSplicing ctx =
  ctx
    { blockSplice = const Nothing
    , inlineSplice = const Nothing
    }

concatSpliceFunc :: Alternative f => (t -> f a) -> (t -> f a) -> t -> f a
concatSpliceFunc f g x =
  asum
    [ f x
    , g x
    ]

rewriteClass :: RenderCtx -> B.Attr -> B.Attr
rewriteClass RenderCtx {..} (id', classes, attr) =
  (id', rewrite classMap <$> classes, attr)
  where
    rewrite :: Ord a => Map a a -> a -> a
    rewrite rules x =
      fromMaybe x $ Map.lookup x rules

blockLookupAttr :: X.Node -> B.Block -> B.Attr
blockLookupAttr node = \case
  B.Para {} -> childTagAttr node "Para"
  B.BulletList {} -> childTagAttr node "BulletList"
  B.OrderedList {} -> childTagAttr node "OrderedList"
  B.CodeBlock {} -> childTagAttr node "CodeBlock"
  B.BlockQuote {} -> childTagAttr node "BlockQuote"
  _ -> B.nullAttr

inlineLookupAttr :: X.Node -> B.Inline -> B.Attr
inlineLookupAttr node = \case
  B.Code {} -> childTagAttr node "Code"
  B.Note _ ->
    childTagAttr node "Note"
  B.Link _ _ (url, _) ->
    fromMaybe B.nullAttr $ do
      link <- X.childElementTag "PandocLink" node
      let innerTag = if "://" `T.isInfixOf` url then "External" else "Internal"
      pure $ attrFromNode link `concatAttr` childTagAttr link innerTag
  _ -> B.nullAttr

childTagAttr :: X.Node -> Text -> B.Attr
childTagAttr x name =
  maybe B.nullAttr attrFromNode $ X.childElementTag name x

attrFromNode :: X.Node -> B.Attr
attrFromNode node =
  let mClass = maybe mempty words $ X.getAttribute "class" node
      id' = fromMaybe "" $ X.getAttribute "id" node
      attrs = filter ((/= "class") . fst) $ X.elementAttrs node
   in (id', mClass, attrs)
