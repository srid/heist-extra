-- | Tests for "Heist.Extra.Splices.Pandoc.Render".
module Heist.Extra.Splices.Pandoc.RenderSpec (spec) where

import Blaze.ByteString.Builder (toByteString)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Heist qualified as H
import Heist.Extra.Splices.Pandoc.Ctx (emptyRenderCtx)
import Heist.Extra.Splices.Pandoc.Render (
  alignmentStyle,
  cellColumnIndices,
  cellSpanAttrs,
  colSpecsToColgroup,
  rawNode,
  rpBlock,
 )
import Test.Hspec
import Text.Pandoc.Builder qualified as PB
import Text.Pandoc.Definition qualified as B
import Text.XmlHtml qualified as X
import Prelude

{- | Force-render a node tree to bytes via xmlhtml's HTML5 fragment renderer.
 Any `error` thrown by the renderer surfaces here.
-}
renderHtml :: [X.Node] -> IO ByteString
renderHtml = evaluate . toByteString . X.renderHtmlFragment X.UTF8

spec :: Spec
spec = do
  describe "rawNode" $ do
    it "wraps content in a <rawhtml> element" $ do
      case rawNode "div" "<svg/>" of
        [X.Element name _ _] -> name `shouldBe` "rawhtml"
        ns -> expectationFailure $ "expected single Element; got " <> show ns

    it "ignores the wrapperTag argument (always emits <rawhtml>)" $ do
      let blockNodes = rawNode "div" "x"
          inlineNodes = rawNode "span" "x"
      blockNodes `shouldBe` inlineNodes

    it "carries the xmlhtmlRaw marker so xmlhtml treats content as opaque" $ do
      case rawNode "div" "<svg/>" of
        [X.Element _ attrs _] ->
          lookup "xmlhtmlRaw" attrs `shouldBe` Just ""
        _ -> expectationFailure "expected single Element"

    it "applies display: contents inline so the wrapper is layout-invisible" $ do
      case rawNode "div" "<svg/>" of
        [X.Element _ attrs _] ->
          lookup "style" attrs `shouldBe` Just "display: contents"
        _ -> expectationFailure "expected single Element"

    it "passes the body through as a single TextNode (no parsing)" $ do
      let body = "<svg><foreignObject><div>x</div></foreignObject></svg>"
      case rawNode "div" body of
        [X.Element _ _ [X.TextNode t]] -> t `shouldBe` body
        ns -> expectationFailure $ "expected one TextNode child; got " <> show ns

  describe "rawNode + xmlhtml render (regression: srid/emanote#119, #625)" $ do
    -- The whole point of switching from <div xmlhtmlRaw> to <rawhtml>: a body
    -- that contains </div> used to crash xmlhtml's renderer at HTML/Render.hs:131
    -- with "div cannot contain text looking like its end tag". With the new
    -- wrapper that check is checking for "</rawhtml" instead, which never
    -- appears in real HTML/SVG content.
    it "renders a body containing </div> without throwing" $ do
      let body = "<details><div>x</div></details>" :: Text
      out <- renderHtml (rawNode "div" body)
      out `shouldSatisfy` (encodeUtf8 body `BS.isInfixOf`)

    it "renders a body containing </span> without throwing (inline path)" $ do
      let body = "<span>a</span><span>b</span>" :: Text
      out <- renderHtml (rawNode "span" body)
      out `shouldSatisfy` (encodeUtf8 body `BS.isInfixOf`)

    it "renders a mermaid-shaped SVG body without throwing" $ do
      -- Shape mirrors mmdc's actual output: outer <svg> with inner
      -- <foreignObject><div>...</div></foreignObject> HTML labels. The </div>
      -- inside foreignObject is what tripped the old wrapper.
      let body =
            "<svg xmlns=\"http://www.w3.org/2000/svg\">\
            \<foreignObject><div xmlns=\"http://www.w3.org/1999/xhtml\">\
            \<span>label</span></div></foreignObject></svg>" ::
              Text
      out <- renderHtml (rawNode "div" body)
      out `shouldSatisfy` ("<foreignObject>" `BS.isInfixOf`)
      out `shouldSatisfy` ("</div>" `BS.isInfixOf`)

  describe "alignmentStyle (srid/emanote#27)" $ do
    it "maps the three explicit alignments to text-align styles" $ do
      alignmentStyle B.AlignLeft `shouldBe` Just ("style", "text-align: left")
      alignmentStyle B.AlignRight `shouldBe` Just ("style", "text-align: right")
      alignmentStyle B.AlignCenter `shouldBe` Just ("style", "text-align: center")

    it "drops AlignDefault entirely instead of emitting an empty style" $
      alignmentStyle B.AlignDefault `shouldBe` Nothing

  describe "colSpecsToColgroup (srid/emanote#27)" $ do
    it "emits nothing when every column width is default" $ do
      let specs = [(B.AlignLeft, B.ColWidthDefault), (B.AlignRight, B.ColWidthDefault)]
      colSpecsToColgroup specs `shouldBe` mempty

    it "emits a <colgroup> with width-bearing <col>s when any width is set" $ do
      let specs = [(B.AlignDefault, B.ColWidth 0.25), (B.AlignDefault, B.ColWidth 0.75)]
      out <- renderHtml (colSpecsToColgroup specs)
      out `shouldSatisfy` ("<colgroup>" `BS.isInfixOf`)
      out `shouldSatisfy` ("width: 25.00%" `BS.isInfixOf`)
      out `shouldSatisfy` ("width: 75.00%" `BS.isInfixOf`)

    it "emits a bare <col> for default-width columns mixed with sized ones" $ do
      let specs = [(B.AlignDefault, B.ColWidth 0.5), (B.AlignDefault, B.ColWidthDefault)]
      out <- renderHtml (colSpecsToColgroup specs)
      out `shouldSatisfy` ("width: 50.00%" `BS.isInfixOf`)
      -- xmlhtml self-closes the void <col> element. The default-width
      -- column emits <col /> (no style attr).
      out `shouldSatisfy` ("<col />" `BS.isInfixOf`)

  describe "cellSpanAttrs (srid/emanote#27)" $ do
    it "emits no attributes when both spans are 1 (HTML default)" $
      cellSpanAttrs (B.RowSpan 1) (B.ColSpan 1) `shouldBe` []

    it "emits rowspan only when the row span is greater than 1" $
      cellSpanAttrs (B.RowSpan 3) (B.ColSpan 1) `shouldBe` [("rowspan", "3")]

    it "emits colspan only when the column span is greater than 1" $
      cellSpanAttrs (B.RowSpan 1) (B.ColSpan 2) `shouldBe` [("colspan", "2")]

    it "emits both attrs when both spans are greater than 1" $
      cellSpanAttrs (B.RowSpan 2) (B.ColSpan 4)
        `shouldBe` [("rowspan", "2"), ("colspan", "4")]

  describe "cellColumnIndices (srid/emanote#27)" $ do
    let plainCell cs = B.Cell B.nullAttr B.AlignDefault (B.RowSpan 1) (B.ColSpan cs) [PB.Plain [PB.Str "x"]]
    it "is a simple [0..] enumeration when every cell spans one column" $
      cellColumnIndices [plainCell 1, plainCell 1, plainCell 1] `shouldBe` [0, 1, 2]

    it "advances by the span when a cell spans multiple columns" $
      -- A 4-column row: cell0 (span 1) | cell1 (span 2 — covers cols 1+2) | cell2 (span 1 — col 3)
      cellColumnIndices [plainCell 1, plainCell 2, plainCell 1] `shouldBe` [0, 1, 3]

    it "handles a leading wide cell" $
      cellColumnIndices [plainCell 3, plainCell 1] `shouldBe` [0, 3]

    it "is empty for an empty row" $
      cellColumnIndices [] `shouldBe` []

  -- Integration: render real Pandoc Table values through `rpBlock` and
  -- assert the bytes that come out of xmlhtml. Covers every variant the
  -- new code path is supposed to honour.
  beforeAll initEmptyHeistState $
    describe "rpBlock B.Table (srid/emanote#27, integration)" $ do
      it "renders a minimal one-cell table with header & body wrappers" $ \hs -> do
        let tbl = simpleTable [(B.AlignDefault, B.ColWidthDefault)] [["H1"]] [[["v1"]]]
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("<table" `BS.isInfixOf`)
        out `shouldSatisfy` ("<thead" `BS.isInfixOf`)
        out `shouldSatisfy` ("<tbody" `BS.isInfixOf`)
        out `shouldSatisfy` ("<th" `BS.isInfixOf`)
        out `shouldSatisfy` ("<td" `BS.isInfixOf`)
        -- No colgroup — every width is default.
        out `shouldSatisfy` (not . ("<colgroup" `BS.isInfixOf`))
        -- No tfoot — no foot rows.
        out `shouldSatisfy` (not . ("<tfoot" `BS.isInfixOf`))

      it "applies per-column alignment to every cell in that column" $ \hs -> do
        -- One header row + one body row, each three cells across.
        -- Each column's alignment must reach two cells.
        let tbl =
              simpleTable
                [ (B.AlignLeft, B.ColWidthDefault)
                , (B.AlignCenter, B.ColWidthDefault)
                , (B.AlignRight, B.ColWidthDefault)
                ]
                [["L", "C", "R"]]
                [[["a", "b", "c"]]]
        out <- renderBlockHtml hs tbl
        bsCount "text-align: left" out `shouldBe` 2
        bsCount "text-align: center" out `shouldBe` 2
        bsCount "text-align: right" out `shouldBe` 2

      it "lets a cell-level Alignment override the column's default" $ \hs -> do
        -- Column 0 is left-aligned; the body cell overrides to right.
        let bodyCell = B.Cell B.nullAttr B.AlignRight (B.RowSpan 1) (B.ColSpan 1) [PB.Plain [PB.Str "x"]]
            tbl =
              tableFromParts
                [(B.AlignLeft, B.ColWidthDefault)]
                [mkRow [mkCell "H"]]
                [[mkRow [bodyCell]]]
                []
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("text-align: right" `BS.isInfixOf`)

      it "emits a <colgroup> with widths when any column has a fixed width" $ \hs -> do
        let tbl =
              simpleTable
                [(B.AlignDefault, B.ColWidth 0.3), (B.AlignDefault, B.ColWidth 0.7)]
                [["H1", "H2"]]
                [[["a"], ["b"]]]
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("<colgroup>" `BS.isInfixOf`)
        out `shouldSatisfy` ("width: 30.00%" `BS.isInfixOf`)
        out `shouldSatisfy` ("width: 70.00%" `BS.isInfixOf`)

      it "emits rowspan / colspan only when greater than 1" $ \hs -> do
        let wide = B.Cell B.nullAttr B.AlignDefault (B.RowSpan 1) (B.ColSpan 2) [PB.Plain [PB.Str "wide"]]
            tall = B.Cell B.nullAttr B.AlignDefault (B.RowSpan 2) (B.ColSpan 1) [PB.Plain [PB.Str "tall"]]
            normal = mkCell "n"
            tbl =
              tableFromParts
                [(B.AlignDefault, B.ColWidthDefault), (B.AlignDefault, B.ColWidthDefault)]
                [mkRow [mkCell "H1", mkCell "H2"]]
                [[mkRow [wide], mkRow [tall, normal]]]
                []
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("colspan='2'" `BS.isInfixOf`)
        out `shouldSatisfy` ("rowspan='2'" `BS.isInfixOf`)
        -- Single-span cells must not carry the attrs.
        out `shouldSatisfy` (not . ("rowspan='1'" `BS.isInfixOf`))
        out `shouldSatisfy` (not . ("colspan='1'" `BS.isInfixOf`))

      it "respects ColSpan when resolving column alignment (regression)" $ \hs -> do
        -- 4 columns: left, center, right, left.
        -- Body row: [span-2 cell, plain cell, plain cell].
        -- The span-2 cell sits at columns 0-1; the next cell must pick up
        -- the *right* alignment (column 2), not the *center* (column 1)
        -- that a naive zip-by-position would yield.
        let bodyCells =
              [ B.Cell B.nullAttr B.AlignDefault (B.RowSpan 1) (B.ColSpan 2) [PB.Plain [PB.Str "spanned"]]
              , mkCell "third"
              , mkCell "fourth"
              ]
            tbl =
              tableFromParts
                [ (B.AlignLeft, B.ColWidthDefault)
                , (B.AlignCenter, B.ColWidthDefault)
                , (B.AlignRight, B.ColWidthDefault)
                , (B.AlignLeft, B.ColWidthDefault)
                ]
                [mkRow [mkCell "A", mkCell "B", mkCell "C", mkCell "D"]]
                [[mkRow bodyCells]]
                []
        out <- renderBlockHtml hs tbl
        -- The body must contain a right-aligned cell ("third"). Without
        -- the colspan-aware index resolution, "third" would inherit
        -- column 1's center alignment instead.
        let bodyHtml = snd $ BS.breakSubstring "<tbody" out
        bodyHtml `shouldSatisfy` ("text-align: right" `BS.isInfixOf`)
        bodyHtml `shouldSatisfy` ("text-align: left" `BS.isInfixOf`)
        bodyHtml `shouldSatisfy` ("third</td>" `BS.isInfixOf`)
        bodyHtml `shouldSatisfy` ("fourth</td>" `BS.isInfixOf`)

      it "renders a table footer when present" $ \hs -> do
        let tbl =
              tableFromParts
                [(B.AlignDefault, B.ColWidthDefault)]
                [mkRow [mkCell "H"]]
                [[mkRow [mkCell "v"]]]
                [mkRow [mkCell "F"]]
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("<tfoot" `BS.isInfixOf`)
        out `shouldSatisfy` ("F</td>" `BS.isInfixOf`)

      it "omits <thead> when the head has no rows" $ \hs -> do
        let tbl =
              tableFromParts
                [(B.AlignDefault, B.ColWidthDefault)]
                []
                [[mkRow [mkCell "v"]]]
                []
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` (not . ("<thead" `BS.isInfixOf`))
        out `shouldSatisfy` ("<tbody" `BS.isInfixOf`)

      it "preserves the table-level Attr on the <table> element" $ \hs -> do
        let attr = ("my-table-id", ["custom-tw"], [("data-foo", "bar")])
            tbl =
              B.Table
                attr
                PB.emptyCaption
                [(B.AlignDefault, B.ColWidthDefault)]
                (B.TableHead B.nullAttr [])
                [B.TableBody B.nullAttr 0 [] [B.Row B.nullAttr [mkCell "v"]]]
                (B.TableFoot B.nullAttr [])
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("id='my-table-id'" `BS.isInfixOf`)
        out `shouldSatisfy` ("custom-tw" `BS.isInfixOf`)
        out `shouldSatisfy` ("data-foo='bar'" `BS.isInfixOf`)

      it "preserves Row Attr on the <tr> element" $ \hs -> do
        let row = B.Row ("row-id", ["row-class"], []) [mkCell "v"]
            tbl =
              tableFromParts
                [(B.AlignDefault, B.ColWidthDefault)]
                []
                [[row]]
                []
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("id='row-id'" `BS.isInfixOf`)
        out `shouldSatisfy` ("row-class" `BS.isInfixOf`)

      it "preserves Cell Attr on the <td> element" $ \hs -> do
        let cell = B.Cell ("cell-id", ["cell-class"], []) B.AlignDefault (B.RowSpan 1) (B.ColSpan 1) [PB.Plain [PB.Str "v"]]
            tbl =
              tableFromParts
                [(B.AlignDefault, B.ColWidthDefault)]
                []
                [[mkRow [cell]]]
                []
        out <- renderBlockHtml hs tbl
        out `shouldSatisfy` ("id='cell-id'" `BS.isInfixOf`)
        out `shouldSatisfy` ("cell-class" `BS.isInfixOf`)

-- * Test helpers for the integration suite.

{- | Build a fresh 'H.HeistState Identity' with no templates loaded.
 'rpBlock' on a table never reaches 'getParamNode' / template lookup,
 so an empty state is sufficient. The namespace is cleared because
 'initHeist' otherwise complains when no @h:@-prefixed splices are
 bound — an inert default that's just noise for these tests.
-}
initEmptyHeistState :: IO (H.HeistState Identity)
initEmptyHeistState = do
  let cfg :: H.HeistConfig Identity
      cfg = H.emptyHeistConfig & H.hcNamespace (const $ Identity "") & runIdentity
  res <- H.initHeist cfg
  case res of
    Left errs -> error $ "initHeist failed: " <> show errs
    Right hs -> pure hs

-- | Render a single Pandoc 'B.Block' to UTF-8 HTML bytes.
renderBlockHtml :: H.HeistState Identity -> B.Block -> IO ByteString
renderBlockHtml hs b =
  let nodes = runIdentity $ H.evalHeistT (rpBlock emptyRenderCtx b) (X.TextNode "") hs
   in renderHtml nodes

-- | A 'Cell' with default alignment, span 1, and plain inline content.
mkCell :: Text -> B.Cell
mkCell t = B.Cell B.nullAttr B.AlignDefault (B.RowSpan 1) (B.ColSpan 1) [PB.Plain [PB.Str t]]

-- | Wrap a list of cells in a default-attr row.
mkRow :: [B.Cell] -> B.Row
mkRow = B.Row B.nullAttr

{- | A 'B.Table' built from plain text head / body / foot rows.
 One row per @[Text]@; each text becomes one default cell.
-}
simpleTable :: [B.ColSpec] -> [[Text]] -> [[[Text]]] -> B.Block
simpleTable specs headRows bodyRows =
  tableFromParts
    specs
    (textRow <$> headRows)
    [textRow <$> rs | rs <- bodyRows]
    []
  where
    textRow = mkRow . fmap mkCell

{- | Same as 'simpleTable' but takes pre-built 'B.Row' values so callers
 can exercise rowspan, colspan, attrs, and per-cell alignment.
-}
tableFromParts ::
  [B.ColSpec] ->
  -- | Head rows.
  [B.Row] ->
  -- | Bodies (each a list of rows).
  [[B.Row]] ->
  -- | Foot rows.
  [B.Row] ->
  B.Block
tableFromParts specs headRows bodyRows footRows =
  B.Table
    B.nullAttr
    PB.emptyCaption
    specs
    (B.TableHead B.nullAttr headRows)
    [B.TableBody B.nullAttr 0 [] rs | rs <- bodyRows]
    (B.TableFoot B.nullAttr footRows)

-- | Substring occurrence count.
bsCount :: ByteString -> ByteString -> Int
bsCount needle = go 0
  where
    nl = BS.length needle
    go n hay
      | BS.null hay = n
      | otherwise =
          let (_, rest) = BS.breakSubstring needle hay
           in if BS.null rest
                then n
                else go (n + 1) (BS.drop nl rest)
