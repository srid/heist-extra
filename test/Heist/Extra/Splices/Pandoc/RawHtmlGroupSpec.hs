{- | Tests for "Heist.Extra.Splices.Pandoc.RawHtmlGroup".

The pass solves @srid/emanote#433@: orphan opener/closer raw-HTML blocks
end up wrapping a markdown paragraph as siblings rather than parents
unless the renderer can see a real DOM 'B.Div' instead of two stranded
'B.RawBlock's. These tests pin the AST shape produced for every variant
that mattered enough to write code for.
-}
module Heist.Extra.Splices.Pandoc.RawHtmlGroupSpec (spec) where

import Data.Text (Text)
import Heist.Extra.Splices.Pandoc.RawHtmlGroup (groupRawHtmlBlocks)
import Test.Hspec
import Text.Pandoc.Definition qualified as B
import Prelude

-- | Block helpers that keep the test cases readable.
raw :: Text -> B.Block
raw s = B.RawBlock (B.Format "html") s

para :: Text -> B.Block
para s = B.Para [B.Str s]

-- | A 'B.Div' with the named tag and no other attrs — what the pass produces.
taggedDiv :: Text -> [B.Block] -> B.Block
taggedDiv tag = B.Div ("", [], [("tag", tag)])

spec :: Spec
spec = describe "groupRawHtmlBlocks (srid/emanote#433)" $ do
  it "groups the issue's example: opener, paragraph, closer" $ do
    -- The exact Pandoc AST emitted by parseNoteMarkdown on the issue's MD.
    let input =
          [ para "aaaa"
          , raw "<details>\n"
          , para "bbb"
          , raw "</details>\n"
          , para "eee"
          ]
        expected =
          [ para "aaaa"
          , taggedDiv "details" [para "bbb"]
          , para "eee"
          ]
    groupRawHtmlBlocks input `shouldBe` expected

  it "produces an empty Div when opener and closer are adjacent" $ do
    -- The "empty group" case the branch is named after: nothing between
    -- the open and close raw blocks.
    let input = [raw "<details>\n", raw "</details>\n"]
        expected = [taggedDiv "details" []]
    groupRawHtmlBlocks input `shouldBe` expected

  it "leaves an opener with no matching closer untouched" $ do
    let input = [raw "<details>\n", para "stuck open"]
    groupRawHtmlBlocks input `shouldBe` input

  it "leaves a closer with no opener untouched" $ do
    let input = [para "lone", raw "</details>\n"]
    groupRawHtmlBlocks input `shouldBe` input

  it "groups consecutive opener/closer pairs independently" $ do
    let input =
          [ raw "<details>\n"
          , para "x"
          , raw "</details>\n"
          , raw "<aside>\n"
          , para "y"
          , raw "</aside>\n"
          ]
        expected =
          [ taggedDiv "details" [para "x"]
          , taggedDiv "aside" [para "y"]
          ]
    groupRawHtmlBlocks input `shouldBe` expected

  it "handles same-tag nesting via depth counting" $ do
    -- Two opens before a close: outer must wrap the inner pair.
    let input =
          [ raw "<details>\n"
          , raw "<details>\n"
          , para "inner"
          , raw "</details>\n"
          , para "between"
          , raw "</details>\n"
          ]
        expected =
          [ taggedDiv
              "details"
              [ taggedDiv "details" [para "inner"]
              , para "between"
              ]
          ]
    groupRawHtmlBlocks input `shouldBe` expected

  it "ignores self-closing forms like <br />" $ do
    let input = [raw "<br />\n", para "after"]
    groupRawHtmlBlocks input `shouldBe` input

  it "ignores raw blocks that already balance internally" $ do
    -- Single-line `<span>x</span>` doesn't get split into open/close
    -- by Pandoc, so it doesn't match the opener pattern.
    let input = [raw "<details>foo</details>\n", para "after"]
    groupRawHtmlBlocks input `shouldBe` input

  it "matches tag names case-insensitively" $ do
    let input = [raw "<Details>\n", para "x", raw "</DETAILS>\n"]
        expected = [taggedDiv "details" [para "x"]]
    groupRawHtmlBlocks input `shouldBe` expected

  it "accepts opener attributes (and drops them on the produced Div)" $ do
    -- Attribute support on the output is out of scope for this pass;
    -- what matters is that the attributes don't trip the parser.
    let input =
          [ raw "<details open class=\"foo\">\n"
          , para "x"
          , raw "</details>\n"
          ]
        expected = [taggedDiv "details" [para "x"]]
    groupRawHtmlBlocks input `shouldBe` expected

  it "groups custom-element tag names with hyphens" $ do
    let input =
          [ raw "<my-card>\n"
          , para "x"
          , raw "</my-card>\n"
          ]
        expected = [taggedDiv "my-card" [para "x"]]
    groupRawHtmlBlocks input `shouldBe` expected

  it "leaves orphan tags whose closers belong to a different element" $ do
    -- An <aside> open with only a </details> downstream stays open.
    let input = [raw "<aside>\n", para "x", raw "</details>\n"]
    groupRawHtmlBlocks input `shouldBe` input
