{- | Internal record definition; the public API and its rationale live in
 "Heist.Extra.Splices.Pandoc.Ctx". Import at your own risk.
-}
module Heist.Extra.Splices.Pandoc.Ctx.Internal (
  RenderCtx (..),
  RenderFeatures (..),
  CodeBackend (..),
  MathBackend (..),
  defaultFeatures,
) where

import Data.Dynamic (Dynamic)
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
