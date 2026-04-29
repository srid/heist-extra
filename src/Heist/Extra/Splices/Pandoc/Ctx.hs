{-# LANGUAGE RecordWildCards #-}

{- | Public construction and manipulation API for 'RenderCtx'.

The data constructor lives in "Heist.Extra.Splices.Pandoc.Ctx.Internal" and is
deliberately not re-exported here: callers must go through 'mkRenderCtx' or
'emptyRenderCtx' so that future fields added to the record can be absorbed by
the smart constructor's defaults without breaking call sites.
-}
module Heist.Extra.Splices.Pandoc.Ctx (
  -- * Type and field accessors
  RenderCtx,
  rootNode,
  bAttr,
  iAttr,
  classMap,
  blockSplice,
  inlineSplice,
  renderFeatures,
  userData,

  -- * Rendering features
  RenderFeatures (..),
  CodeBackend (..),
  MathBackend (..),
  defaultFeatures,

  -- * Smart constructors
  mkRenderCtx,
  emptyRenderCtx,

  -- * Typed user-data slot
  getUserData,
  setUserData,

  -- * Other helpers
  rewriteClass,
  ctxSansCustomSplicing,
  concatSpliceFunc,
) where

import Data.Dynamic (fromDynamic, toDyn)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Heist qualified as H
import Heist.Extra.Splices.Pandoc.Attr (concatAttr)
import Heist.Extra.Splices.Pandoc.Ctx.Internal (
  CodeBackend (..),
  MathBackend (..),
  RenderCtx (..),
  RenderFeatures (..),
  defaultFeatures,
 )
import Heist.Interpreted qualified as HI
import Text.Pandoc.Builder qualified as B
import Text.XmlHtml qualified as X

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

concatSpliceFunc :: (Alternative f) => (t -> f a) -> (t -> f a) -> t -> f a
concatSpliceFunc f g x =
  asum
    [ f x
    , g x
    ]

rewriteClass :: RenderCtx -> B.Attr -> B.Attr
rewriteClass RenderCtx {..} (id', classes, attr) =
  (id', rewrite classMap <$> classes, attr)
  where
    rewrite :: (Ord a) => Map a a -> a -> a
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
