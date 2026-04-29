{- | Pure helpers powering the Pandoc 'Text.Pandoc.Definition.Table' arm of
 "Heist.Extra.Splices.Pandoc.Render". Exposed so the unit suite can pin
 their behaviour without standing up a Heist splice context. Not part of
 the library's public surface — import at your own risk.
-}
module Heist.Extra.Splices.Pandoc.Render.Internal (
  alignmentStyle,
  colSpecsToColgroup,
  cellSpanAttrs,
  cellColumnIndices,
  mergeStyleKVs,

  -- * The @"tag"@ directive on 'B.Div'
  tagDirectiveKey,
  divTag,
  stripTagDirective,
) where

import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Numeric (showFFloat)
import Text.Pandoc.Definition qualified as B
import Text.XmlHtml qualified as X

{- | 'B.AlignDefault' yields 'Nothing' so the attribute is omitted entirely
 rather than emitting an inline style that re-asserts the user-agent default.
-}
alignmentStyle :: B.Alignment -> Maybe (Text, Text)
alignmentStyle = \case
  B.AlignLeft -> Just ("style", "text-align: left")
  B.AlignRight -> Just ("style", "text-align: right")
  B.AlignCenter -> Just ("style", "text-align: center")
  B.AlignDefault -> Nothing

{- | Returns the empty list when every width is 'B.ColWidthDefault' — a
 @\<colgroup\>@ full of bare @\<col\>@s would just be HTML noise.
-}
colSpecsToColgroup :: [B.ColSpec] -> [X.Node]
colSpecsToColgroup specs
  | all ((== B.ColWidthDefault) . snd) specs = mempty
  | otherwise = one $ X.Element "colgroup" mempty (renderCol <$> specs)
  where
    renderCol (_, B.ColWidthDefault) = X.Element "col" mempty mempty
    renderCol (_, B.ColWidth w) =
      X.Element "col" [("style", "width: " <> percent w)] mempty
    percent w = T.pack (showFFloat (Just 2) (w * 100) "") <> "%"

-- | Spans of 1 are the HTML default and are omitted to avoid noise.
cellSpanAttrs :: B.RowSpan -> B.ColSpan -> [(Text, Text)]
cellSpanAttrs (B.RowSpan rs) (B.ColSpan cs) =
  catMaybes
    [ guard (rs > 1) $> ("rowspan", show rs)
    , guard (cs > 1) $> ("colspan", show cs)
    ]

{- | A cell with @ColSpan n@ occupies indices @[i .. i+n-1]@, so the next
 cell starts at @i+n@; zipping by list position would silently misalign
 every cell after a merged one.
-}
cellColumnIndices :: [B.Cell] -> [Int]
cellColumnIndices = go 0
  where
    go _ [] = []
    go col (B.Cell _ _ _ (B.ColSpan cs) _ : rest) = col : go (col + cs) rest

{- | Append @right@ onto @left@, but consolidate any duplicate @style@ keys
 into one entry joined with @"; "@. xmlhtml passes attributes through
 verbatim, so two @style@ entries reach the browser as two attributes on
 one element — invalid HTML, with the later value silently dropped by
 most engines.
-}
mergeStyleKVs :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
mergeStyleKVs left right =
  let (styles, rest) = partition ((== "style") . fst) (left <> right)
   in case styles of
        [] -> rest
        [single] -> rest <> [single]
        multiple -> rest <> [("style", T.intercalate "; " (snd <$> multiple))]

{- | The single attribute key that callers (today: 'Heist.Extra.Splices.Pandoc.RawHtmlGroup')
use as a directive on a 'B.Div' to override the rendered element name. The
renderer reads it via 'divTag' and removes it via 'stripTagDirective' so
the directive never leaks into serialised HTML as a literal @tag="…"@.
A named constant rather than three string literals scattered across
modules: future maintainers can grep one place.
-}
tagDirectiveKey :: Text
tagDirectiveKey = "tag"

{- | Resolve the element name a 'B.Div' should render as: the directive value if
 present, else the supplied default.
-}
divTag :: Text -> B.Attr -> Text
divTag defaultTag (_, _, kvs) =
  fromMaybe defaultTag (Map.lookup tagDirectiveKey (Map.fromList kvs))

-- | Drop the directive from a 'B.Attr' so it doesn't survive into rendered HTML.
stripTagDirective :: B.Attr -> B.Attr
stripTagDirective (i, cs, kvs) =
  (i, cs, filter ((/= tagDirectiveKey) . fst) kvs)
