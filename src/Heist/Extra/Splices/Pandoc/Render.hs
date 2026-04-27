{-# LANGUAGE RecordWildCards #-}

module Heist.Extra.Splices.Pandoc.Render (
  renderPandocWith,
  rpBlock,
  rpInline,
  rpBlock',
  rpInline',

  -- * Internal helpers (exported for tests)
  rawNode,
  alignmentStyle,
  colSpecsToColgroup,
  cellSpanAttrs,
  cellColumnIndices,
) where

import Data.Map.Strict qualified as Map
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Heist qualified as H
import Heist.Extra (runCustomNode)
import Heist.Extra.Splices.Pandoc.Attr (concatAttr, rpAttr)
import Heist.Extra.Splices.Pandoc.Ctx (
  CodeBackend (..),
  MathBackend (..),
  RenderCtx (..),
  RenderFeatures (..),
  rewriteClass,
 )
import Heist.Extra.Splices.Pandoc.Skylighting (highlightCode)
import Heist.Extra.Splices.Pandoc.TaskList qualified as TaskList
import Heist.Extra.Splices.Pandoc.Texmath (renderMath)
import Heist.Interpreted qualified as HI
import Numeric (showFFloat)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Walk as W
import Text.XmlHtml qualified as X

renderPandocWith :: RenderCtx -> Pandoc -> HI.Splice Identity
renderPandocWith ctx (Pandoc _meta blocks) =
  foldMapM (rpBlock ctx) blocks

rpBlock :: RenderCtx -> B.Block -> HI.Splice Identity
rpBlock ctx@RenderCtx {..} b = do
  fromMaybe (rpBlock' ctx b) $ blockSplice b

-- | Render using user override in pandoc.tpl, falling back to default HTML.
withTplTag :: RenderCtx -> Text -> H.Splices (HI.Splice Identity) -> HI.Splice Identity -> HI.Splice Identity
withTplTag RenderCtx {..} name splices default_ =
  case X.childElementTag name =<< rootNode of
    Nothing -> default_
    Just node -> runCustomNode node splices

rpBlock' :: RenderCtx -> B.Block -> HI.Splice Identity
rpBlock' ctx@RenderCtx {..} b = case b of
  B.Plain is ->
    rpInlineWithTasks ctx (convertRawInline [] is)
  B.Para is -> do
    let innerSplice = rpInlineWithTasks ctx (convertRawInline [] is)
    withTplTag ctx "Para" ("inlines" ## innerSplice) $
      one . X.Element "p" mempty <$> innerSplice
  B.LineBlock iss ->
    flip foldMapM iss $ \is ->
      foldMapM (rpInline ctx) (convertRawInline [] is) >> pure [X.TextNode "\n"]
  B.CodeBlock (id', classes, attrs) s -> do
    let lang = listToMaybe classes
        codeNodes = case codeHighlighting renderFeatures of
          Skylighting -> case highlightCode lang s of
            Left err -> [X.Element "span" [("class", "error")] [X.TextNode err], X.TextNode s]
            Right nodes -> nodes
          NoHighlighting -> one $ X.TextNode s
    pure $
      one . X.Element "div" (rpAttr $ bAttr b) $
        one . X.Element "pre" mempty $
          one . X.Element "code" (rpAttr (id', classes, attrs)) $
            codeNodes
  B.RawBlock (B.Format fmt) s -> do
    pure $ case fmt of
      "html" ->
        rawNode "div" s
      _ ->
        one . X.Element "pre" [("class", "pandoc-raw-" <> show fmt)] $ one . X.TextNode $ s
  B.BlockQuote bs ->
    withTplTag ctx "BlockQuote" ("blocks" ## rpBlock ctx `foldMapM` bs) $
      one . X.Element "blockquote" mempty <$> foldMapM (rpBlock ctx) bs
  B.OrderedList _ bss ->
    withTplTag ctx "OrderedList" (pandocListSplices "OrderedList" bss) $ do
      fmap (one . X.Element "ol" (rpAttr $ bAttr b)) $
        flip foldMapM bss $
          fmap (one . X.Element "li" mempty) . foldMapM (rpBlock ctx)
  B.BulletList bss ->
    withTplTag ctx "BulletList" (pandocListSplices "BulletList" bss) $ do
      fmap (one . X.Element "ul" (rpAttr $ bAttr b)) $
        flip foldMapM bss $
          fmap (one . X.Element "li" mempty) . foldMapM (rpBlock ctx)
  B.DefinitionList defs ->
    withTplTag ctx "DefinitionList" (definitionListSplices defs) $
      fmap (one . X.Element "dl" mempty) $
        flip foldMapM defs $ \(term, descList) -> do
          a <- foldMapM (rpInline ctx) (convertRawInline [] term)
          as <-
            flip foldMapM descList $
              fmap (one . X.Element "dd" mempty) . foldMapM (rpBlock ctx)
          pure $ a <> as
  B.Header level attr@(headerId, _, _) is -> do
    let innerSplice = foldMapM (rpInline ctx) (convertRawInline [] is)
    withTplTag ctx ("Header:" <> show level) (headerSplices headerId innerSplice) $
      one . X.Element (headerTag level) (rpAttr $ concatAttr attr $ bAttr b)
        <$> innerSplice
  B.HorizontalRule ->
    withTplTag ctx "HorizontalRule" mempty (pure $ one $ X.Element "hr" mempty mempty)
  B.Table attr _captions colSpecs (B.TableHead _ hrows) tbodys (B.TableFoot _ frows) -> do
    -- TODO: Move tailwind styles to pandoc.tpl
    let borderStyle = "border-gray-300"
        rowTwAttr = ("", ["border-b-2", "border-t-2", borderStyle], mempty)
        cellTwAttr = ("", ["py-2", "px-2", "align-top", "border-r-2", "border-l-2", borderStyle], mempty)
        tableAttr = ("", ["mb-3"], mempty)
        colAligns = fst <$> colSpecs
        colAlignAt i = case drop i colAligns of
          a : _ -> a
          [] -> B.AlignDefault
        renderCell tag i (B.Cell cAttr cAlign rspan cspan blks) = do
          let effectiveAlign = case cAlign of
                B.AlignDefault -> colAlignAt i
                a -> a
              extraKVs = catMaybes [alignmentStyle effectiveAlign] <> cellSpanAttrs rspan cspan
              merged = concatAttr (concatAttr cAttr cellTwAttr) ("", mempty, extraKVs)
          one . X.Element tag (rpAttr merged) <$> foldMapM (rpBlock ctx) blks
        renderRow tag (B.Row rAttr cells) = do
          -- Cell *position* in the row and column *index* diverge as soon as
          -- any cell has ColSpan > 1; resolving alignment by zip-position
          -- would silently misfire across a merged cell.
          rendered <- fold <$> zipWithM (renderCell tag) (cellColumnIndices cells) cells
          pure $ one $ X.Element "tr" (rpAttr (concatAttr rAttr rowTwAttr)) rendered
        wrapSection tag cellTag rows
          | null rows = pure mempty
          | otherwise = one . X.Element tag mempty <$> foldMapM (renderRow cellTag) rows
    fmap (one . X.Element "table" (rpAttr $ concatAttr attr tableAttr)) $ do
      let cg = colSpecsToColgroup colSpecs
      thead <- wrapSection "thead" "th" hrows
      tbody <- fmap (one . X.Element "tbody" mempty) $
        flip foldMapM tbodys $ \(B.TableBody _ _ _ rows) ->
          foldMapM (renderRow "td") rows
      tfoot <- wrapSection "tfoot" "td" frows
      pure $ cg <> thead <> tbody <> tfoot
  B.Div attr bs ->
    one . X.Element (getTag "div" attr) (rpAttr $ rewriteClass ctx attr)
      <$> foldMapM (rpBlock ctx) bs
  B.Figure attr _caption bs ->
    -- TODO: support caption
    one . X.Element "figure" (rpAttr attr) <$> foldMapM (rpBlock ctx) bs
  where
    getTag defaultTag (_, _, Map.fromList -> attrs) =
      Map.lookup "tag" attrs & fromMaybe defaultTag

    headerSplices headerId innerSplice = do
      "header:id" ## HI.textSplice headerId
      "inlines" ## innerSplice

    definitionListSplices :: [([B.Inline], [[B.Block]])] -> H.Splices (HI.Splice Identity)
    definitionListSplices defs = do
      "DefinitionList:Items" ## (HI.runChildrenWith . uncurry itemsSplices) `foldMapM` defs
      where
        itemsSplices :: [B.Inline] -> [[B.Block]] -> H.Splices (HI.Splice Identity)
        itemsSplices term descriptions = do
          "DefinitionList:Item:Term" ## foldMapM (rpInline ctx) term
          "DefinitionList:Item:DescList" ## (HI.runChildrenWith . descListSplices) `foldMapM` descriptions
        descListSplices :: [B.Block] -> H.Splices (HI.Splice Identity)
        descListSplices bs = "DefinitionList:Item:Desc" ## rpBlock ctx `foldMapM` bs

    pandocListSplices :: Text -> [[B.Block]] -> H.Splices (HI.Splice Identity)
    pandocListSplices tagPrefix bss =
      (tagPrefix <> ":Items") ## (HI.runChildrenWith . itemsSplices) `foldMapM` bss
      where
        itemsSplices :: [B.Block] -> H.Splices (HI.Splice Identity)
        itemsSplices bs = do
          (tagPrefix <> ":Item") ## foldMapM (rpBlock ctx) bs

headerTag :: HasCallStack => Int -> Text
headerTag n =
  if n >= 1 && n <= 6
    then "h" <> show n
    else error "Invalid pandoc header level"

rpInline :: RenderCtx -> B.Inline -> HI.Splice Identity
rpInline ctx@RenderCtx {..} i = do
  fromMaybe (rpInline' ctx i) $ inlineSplice i

rpInline' :: RenderCtx -> B.Inline -> HI.Splice Identity
rpInline' ctx@RenderCtx {..} i = case i of
  B.Str s ->
    pure $ one . X.TextNode $ s
  B.Emph is ->
    one . X.Element "em" mempty <$> foldMapM (rpInline ctx) is
  B.Strong is ->
    one . X.Element "strong" mempty <$> foldMapM (rpInline ctx) is
  B.Underline is ->
    one . X.Element "u" mempty <$> foldMapM (rpInline ctx) is
  B.Strikeout is ->
    one . X.Element "s" mempty <$> foldMapM (rpInline ctx) is
  B.Superscript is ->
    one . X.Element "sup" mempty <$> foldMapM (rpInline ctx) is
  B.Subscript is ->
    one . X.Element "sub" mempty <$> foldMapM (rpInline ctx) is
  B.Quoted qt is ->
    flip inQuotes qt $ foldMapM (rpInline ctx) is
  B.Code attr s ->
    pure $
      one . X.Element "code" (rpAttr $ concatAttr attr $ iAttr i) $
        one . X.TextNode $
          s
  B.Space -> pure $ one . X.TextNode $ " "
  B.SoftBreak -> pure $ one . X.TextNode $ " "
  B.LineBreak ->
    pure $ one $ X.Element "br" mempty mempty
  B.RawInline (B.Format fmt) s ->
    if fmt == "html"
      then pure $ rawNode "span" s
      else
        pure $
          one . X.Element "pre" [("class", "pandoc-raw-" <> show fmt)] $
            one . X.TextNode $
              s
  B.Math mathType s ->
    pure $ case mathRendering renderFeatures of
      StaticMathML -> case renderMath mathType s of
        Right nodes -> nodes
        Left err ->
          one . X.Element "span" [("class", "math error")] $
            [X.TextNode err, X.TextNode ": ", X.TextNode s]
      NoStaticMath -> renderMathPassthrough mathType s
  B.Link attr is (url, tit) -> do
    let attrs =
          catMaybes [Just ("href", url), guard (not $ T.null tit) >> pure ("title", tit)]
            <> rpAttr (concatAttr attr $ iAttr i)
    one . X.Element "a" attrs <$> foldMapM (rpInline ctx) is
  B.Image attr is (url, tit) -> do
    let attrs =
          catMaybes
            [ pure ("src", url)
            , guard (not $ T.null tit) >> pure ("title", tit)
            , pure ("alt", plainify is)
            ]
            <> rpAttr (rewriteClass ctx attr)
    pure $ one . X.Element "img" attrs $ mempty
  B.Note _bs -> do
    -- Footnotes are to be handled separately; see Footenotes.hs
    pure $ one $ X.Element "sup" mempty $ one $ X.TextNode "*"
  B.Span attr is -> do
    one . X.Element "span" (rpAttr $ rewriteClass ctx attr) <$> foldMapM (rpInline ctx) is
  B.SmallCaps is ->
    foldMapM (rpInline ctx) is
  B.Cite _citations is ->
    -- TODO: What to do with _citations here?
    withTplTag ctx "Cite" ("inlines" ## rpInline ctx `foldMapM` is) $
      one . X.Element "cite" mempty <$> foldMapM (rpInline ctx) is
  where
    inQuotes :: HI.Splice Identity -> B.QuoteType -> HI.Splice Identity
    inQuotes w = \case
      B.SingleQuote ->
        w <&> \nodes ->
          [X.TextNode "‘"] <> nodes <> [X.TextNode "’"]
      B.DoubleQuote ->
        w <&> \nodes ->
          [X.TextNode "“"] <> nodes <> [X.TextNode "”"]

{- | Convert raw html attribute sequence into span

 For example, this markdown: `<kbd>ctrl</kbd>`, which is in native pandoc:
   RawInline (Format "html") "<kbd>" : Str "ctrl" : RawInline (Format "html") "</kbd>"
 … is converted as:
   <span xmlhtmlraw=""><kbd>ctrl</kbd></span>
 … instead of the default behavior which is:
   <span xmlhtmlraw=""><kbd></kbd></span>ctrl<span xmlhtmlraw=""></span>
-}
convertRawInline :: [B.Inline] -> [B.Inline] -> [B.Inline]
convertRawInline acc = \case
  [] -> reverse acc
  (B.RawInline (B.Format "html") oTag : rest)
    | -- This is a new raw tag, let's find a matching closing tag
      Just (newElem, is) <- mkHtmlInline oTag rest ->
        convertRawInline (newElem : acc) is
  i : is -> convertRawInline (i : acc) is
  where
    mkHtmlInline :: Text -> [B.Inline] -> Maybe (B.Inline, [B.Inline])
    mkHtmlInline oTag rest = case span (not . isClosingTag oTag) rest of
      -- Collect the inner element until a closing tag
      (inner, (closing : is))
        | -- Verify we did find a matching tag
          isClosingTag oTag closing ->
            let inner' = oTag <> plainify inner <> "</" <> T.drop 1 oTag
             in Just (B.RawInline (B.Format "html") inner', is)
      _ -> Nothing

    isClosingTag :: Text -> B.Inline -> Bool
    isClosingTag oTag = \case
      B.RawInline (B.Format "html") eTag ->
        "<" `T.isPrefixOf` oTag && "</" `T.isPrefixOf` eTag && T.drop 1 oTag == T.drop 2 eTag
      _ -> False

-- | Like rpInline', but supports task checkbox in the given inlines.
rpInlineWithTasks :: RenderCtx -> [B.Inline] -> HI.Splice Identity
rpInlineWithTasks ctx is =
  rpTask ctx is $
    rpInline ctx `foldMapM` is

rpTask :: RenderCtx -> [B.Inline] -> HI.Splice Identity -> HI.Splice Identity
rpTask ctx is default_ =
  maybe default_ render (TaskList.parseTaskFromInlines is)
  where
    render (checked, taskInlines) = do
      let tag = bool "Task:Unchecked" "Task:Checked" checked
      withTplTag
        ctx
        tag
        ("inlines" ## rpInline ctx `foldMapM` taskInlines)
        default_

{- | Wrap a raw HTML/inline blob in a Heist-internal element so xmlhtml's
parser keeps the bytes opaque (via the @xmlhtmlRaw@ marker attribute) and
the renderer emits them verbatim.

The wrapper tag is a fixed @rawhtml@ regardless of caller intent: xmlhtml's
@Render.hs:131-133@ check fires if a raw-text element's text content
contains @</tagName@, so wrapping in @\<div\>@ blows up on any embedded
HTML that includes a closing @\</div\>@ — most painfully, mermaid's SVG
ships HTML labels inside @\<foreignObject\>@. A single dedicated tag
name avoids the collision once and for all. @display: contents@ collapses
the wrapper so it doesn't disturb the surrounding flow layout.
-}
rawNode :: Text -> Text -> [X.Node]
rawNode _wrapperTag s =
  one
    . X.Element
      "rawhtml"
      [("xmlhtmlRaw", ""), ("style", "display: contents")]
    $ one . X.TextNode
    $ s

-- | Emit raw LaTeX wrapped in a span for client-side rendering (KaTeX/MathJax).
renderMathPassthrough :: B.MathType -> Text -> [X.Node]
renderMathPassthrough mathType s =
  one . X.Element "span" [("class", klass)] . one . X.TextNode $ wrapped
  where
    (klass, wrapped) = case mathType of
      B.InlineMath -> ("math inline", "\\(" <> s <> "\\)")
      B.DisplayMath -> ("math display", "$$" <> s <> "$$")

{- | Render the per-column @text-align@ inline style for a Pandoc
 'B.Alignment'. 'B.AlignDefault' yields 'Nothing' so the attribute is
 omitted entirely instead of inheriting the user agent's default
 explicitly.
-}
alignmentStyle :: B.Alignment -> Maybe (Text, Text)
alignmentStyle = \case
  B.AlignLeft -> Just ("style", "text-align: left")
  B.AlignRight -> Just ("style", "text-align: right")
  B.AlignCenter -> Just ("style", "text-align: center")
  B.AlignDefault -> Nothing

{- | Build a @\<colgroup\>@ from Pandoc 'B.ColSpec' widths. Returns an
 empty list when every column width is 'B.ColWidthDefault' so we don't
 emit a noise-only element.
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

{- | HTML @rowspan@ / @colspan@ attribute pairs for spans greater than 1.
 Spans of 1 are the HTML default and are omitted.
-}
cellSpanAttrs :: B.RowSpan -> B.ColSpan -> [(Text, Text)]
cellSpanAttrs (B.RowSpan rs) (B.ColSpan cs) =
  catMaybes
    [ guard (rs > 1) $> ("rowspan", show rs)
    , guard (cs > 1) $> ("colspan", show cs)
    ]

{- | The starting column index of every cell in a row, accounting for
 'B.ColSpan'. A cell that spans /n/ columns occupies indices
 @[i .. i+n-1]@; the next cell starts at @i+n@. Resolving column
 alignment by simple list position would silently misalign cells
 whenever a row contains a merged cell.
-}
cellColumnIndices :: [B.Cell] -> [Int]
cellColumnIndices = go 0
  where
    go _ [] = []
    go col (B.Cell _ _ _ (B.ColSpan cs) _ : rest) = col : go (col + cs) rest

-- | Convert Pandoc AST inlines to raw text.
plainify :: [B.Inline] -> Text
plainify = W.query $ \case
  B.Str x -> x
  B.Code _attr x -> x
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> " "
  -- TODO: if fmt is html, we should strip the html tags
  B.RawInline _fmt s -> s
  -- Ignore "wrapper" inlines like span.
  B.Span _ _ -> ""
  -- TODO: How to wrap math stuff here?
  B.Math _mathTyp s -> s
  _ -> ""
