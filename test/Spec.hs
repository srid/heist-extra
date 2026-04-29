module Main (main) where

import Heist.Extra.Splices.Pandoc.CtxSpec qualified as CtxSpec
import Heist.Extra.Splices.Pandoc.RawHtmlGroupSpec qualified as RawHtmlGroupSpec
import Heist.Extra.Splices.Pandoc.RenderSpec qualified as RenderSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  RenderSpec.spec
  RawHtmlGroupSpec.spec
  CtxSpec.spec
