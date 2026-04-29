{- | Group orphan raw-HTML opener / closer block pairs around the markdown
content between them, so the renderer can emit a real DOM element that
nests the content as children.

Background. CommonMark "type 6" HTML blocks (any block-level start tag) end
at the next blank line. Markdown like

@
\<details\>

**bold** content

\</details\>
@

reaches a Pandoc renderer as three blocks: a 'B.RawBlock' with
@"\<details\>\\n"@, a 'B.Para' for the paragraph, and another 'B.RawBlock'
with @"\</details\>\\n"@. Heist-extra's renderer wraps each raw blob in
its own @\<rawhtml\>@ element to keep xmlhtml from mangling the bytes; the
side effect is that the @\<details\>@ open and close get trapped inside
those wrappers and the markdown paragraph ends up a sibling of the (now
empty) details element rather than its child. See @srid/emanote#433@.

This pass walks a block list and, when it sees an unbalanced opening tag
followed downstream by a matching closing tag (depth counted only against
opens of the same tag name), replaces that span with a 'B.Div' carrying
the tag name in the @"tag"@ attribute. The render path already turns @Div@
with a @"tag"@ attr into the named element, so the markdown content lands
as a real DOM child.

Tags without a matching closer are left as raw blocks — emitting a
synthetic close would change the input's semantics. Tag names are
compared case-insensitively to follow HTML's own rules. Attributes on
the opener (e.g. @\<details open\>@) are dropped when we group; that is
deliberate scope until a real case demands otherwise. A tag like
@\<details\>foo\</details\>@ that already balances inside one
'B.RawBlock' is left alone — the renderer handles balanced raw-HTML
fragments correctly today.

== Volatility & boundary

This module exists to encapsulate one specific axis of change: the
strategy for rebalancing orphan raw-HTML blocks around CommonMark "type 6"
content. Future evolution (alternative wrapping strategies, attribute
preservation on the produced 'B.Div', void-element awareness, support for
new HTML element families, smarter nesting heuristics) lives here so the
renderer's interface stays stable. Treat the boundary as load-bearing —
do not inline @groupRawHtmlBlocks@ into the renderer or scatter pieces
of the parsing logic across other modules.

== Public surface

The module is exposed in @heist-extra.cabal@ for downstream tests and
ad-hoc tooling that wants to drive the same preprocessing without going
through 'Heist.Extra.Splices.Pandoc.Render.renderPandocWith'. It is not
covered by the library's stability guarantees; the API can change
between minor versions without a deprecation cycle. Mirrors the
arrangement of "Heist.Extra.Splices.Pandoc.Render.Internal".
-}
module Heist.Extra.Splices.Pandoc.RawHtmlGroup (
  groupRawHtmlBlocks,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text qualified as T
import Heist.Extra.Splices.Pandoc.Render.Internal (tagDirectiveKey)
import Text.Pandoc.Definition qualified as B

groupRawHtmlBlocks :: [B.Block] -> [B.Block]
groupRawHtmlBlocks = \case
  [] -> []
  b : rest
    | Just tag <- openerTag b
    , Just (inner, after) <- splitAtMatchingCloser tag rest ->
        B.Div ("", [], [(tagDirectiveKey, tag)]) (groupRawHtmlBlocks inner)
          : groupRawHtmlBlocks after
    | otherwise -> b : groupRawHtmlBlocks rest

-- | Read @b@ as an opening raw-HTML tag, returning the (lowercased) tag name.
openerTag :: B.Block -> Maybe Text
openerTag = \case
  B.RawBlock (B.Format "html") s -> parseOpener (T.strip s)
  _ -> Nothing
  where
    parseOpener s = do
      afterLT <- T.stripPrefix "<" s
      guard $ not ("/" `T.isPrefixOf` afterLT)
      let (name, rest) = T.span isTagNameChar afterLT
      guard $ not (T.null name)
      -- Reject self-closing forms (@\<br /\>@): the last non-space char
      -- before '>' is '/'.
      let inside = T.takeWhile (/= '>') rest
      guard $ not ("/" `T.isSuffixOf` T.stripEnd inside)
      -- The opener must be the only thing on this raw block; if anything
      -- non-whitespace follows the '>', we're looking at a single-block
      -- balanced fragment and must not group it.
      afterGT <- T.stripPrefix ">" (T.dropWhile (/= '>') rest)
      guard $ T.all isSpace afterGT
      pure $ T.toLower name

-- | Read @b@ as a closing tag for @tag@.
closerTag :: Text -> B.Block -> Bool
closerTag tag = \case
  B.RawBlock (B.Format "html") s -> case T.stripPrefix "</" (T.strip s) of
    Just rest ->
      let (name, after) = T.span isTagNameChar rest
          -- Mirror 'parseOpener': require an explicit @\>@ rather than
          -- silently skipping a missing one via @T.drop 1@. Without this,
          -- a malformed @\</details@ (no @\>@) would still match.
          afterClose = T.stripPrefix ">" (T.dropWhile (/= '>') after)
       in T.toLower name == tag && maybe False (T.all isSpace) afterClose
    Nothing -> False
  _ -> False

isTagNameChar :: Char -> Bool
isTagNameChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '-'

{- | Walk forward, tracking nesting depth of @tag@. On the matching closer
(depth back to zero) split into the inner span and the remainder. 'Nothing'
means the opener is orphan at this level.
-}
splitAtMatchingCloser :: Text -> [B.Block] -> Maybe ([B.Block], [B.Block])
splitAtMatchingCloser tag = go (1 :: Int) []
  where
    go _ _ [] = Nothing
    go depth acc (b : bs)
      | closerTag tag b =
          if depth == 1
            then Just (reverse acc, bs)
            else go (depth - 1) (b : acc) bs
      | Just t <- openerTag b
      , t == tag =
          go (depth + 1) (b : acc) bs
      | otherwise = go depth (b : acc) bs
