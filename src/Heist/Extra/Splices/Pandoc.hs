module Heist.Extra.Splices.Pandoc (
  RenderCtx (..),
  pandocSplice,
  pandocSpliceWithoutFootnoteList,

  -- | Render a `Pandoc` doc with no splice wiring beyond what the given
  -- context already holds. Exposed for callers that take over footnote
  -- handling themselves.
  renderPandocWith,
  -- | To delegate rendering of blocks and inlines from a custom splice.
  rpBlock,
  rpInline,
) where

import Heist.Extra.Splices.Pandoc.Ctx (
  RenderCtx (..),
  concatSpliceFunc,
 )
import Heist.Extra.Splices.Pandoc.Footnotes (
  footnoteRefSplice,
  gatherFootnotes,
  renderFootnotesWith,
 )
import Heist.Extra.Splices.Pandoc.Render (
  renderPandocWith,
  rpBlock,
  rpInline,
 )
import Heist.Interpreted qualified as HI
import Text.Pandoc.Definition (Pandoc (..))

-- | A splice to render a Pandoc AST. Emits the document body followed
-- by a trailing `<ol>` of footnote definitions.
pandocSplice ::
  RenderCtx ->
  Pandoc ->
  HI.Splice Identity
pandocSplice ctx doc = do
  docNodes <- pandocSpliceWithoutFootnoteList ctx doc
  footnotesNodes <- renderFootnotesWith ctx (gatherFootnotes doc)
  pure $ docNodes <> footnotesNodes

{- | Like `pandocSplice` but omits the trailing footnote list.

 Footnote references inside the rendered doc are still wired (each
 `B.Note` becomes a `<sup>` via the `footnote:*` splices), but no
 `<ol>` of definitions is emitted at the end.

 Intended for custom block renderers that are themselves invoked from
 inside an outer `pandocSplice` and that want to render a sub-`Pandoc`
 (e.g. a callout body) without producing a duplicate footnote listing.
 The outer `pandocSplice` already walks the full AST via
 `gatherFootnotes` and emits the document-level list; this function
 relies on that single list instead of emitting its own.
-}
pandocSpliceWithoutFootnoteList ::
  RenderCtx ->
  Pandoc ->
  HI.Splice Identity
pandocSpliceWithoutFootnoteList ctx doc = do
  let footnotes = gatherFootnotes doc
      docCtx =
        ctx
          { inlineSplice = concatSpliceFunc (inlineSplice ctx) (footnoteRefSplice docCtx footnotes)
          }
  renderPandocWith docCtx doc
