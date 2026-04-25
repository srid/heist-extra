-- | Tests for "Heist.Extra.Splices.Pandoc.Render".
module Heist.Extra.Splices.Pandoc.RenderSpec (spec) where

import Blaze.ByteString.Builder (toByteString)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Heist.Extra.Splices.Pandoc.Render (rawNode)
import Prelude
import Test.Hspec
import Text.XmlHtml qualified as X

-- | Force-render a node tree to bytes via xmlhtml's HTML5 fragment renderer.
-- Any `error` thrown by the renderer surfaces here.
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
            \<span>label</span></div></foreignObject></svg>" :: Text
      out <- renderHtml (rawNode "div" body)
      out `shouldSatisfy` ("<foreignObject>" `BS.isInfixOf`)
      out `shouldSatisfy` ("</div>" `BS.isInfixOf`)
