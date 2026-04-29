{-# LANGUAGE TypeApplications #-}

{- | Tests for "Heist.Extra.Splices.Pandoc.Ctx" — specifically the typed
@userData@ slot that lets downstream callers attach per-render state.
-}
module Heist.Extra.Splices.Pandoc.CtxSpec (spec) where

import Heist.Extra.Splices.Pandoc.Ctx (
  emptyRenderCtx,
  getUserData,
  setUserData,
 )
import Test.Hspec
import Prelude

newtype EmbedStack = EmbedStack [Int]
  deriving (Eq, Show)

newtype OtherTag = OtherTag Int
  deriving (Eq, Show)

spec :: Spec
spec = describe "userData slot" $ do
  it "starts empty: getUserData on a fresh ctx returns Nothing for any type" $ do
    getUserData @EmbedStack emptyRenderCtx `shouldBe` Nothing
    getUserData @OtherTag emptyRenderCtx `shouldBe` Nothing

  it "round-trips a typed value through setUserData / getUserData" $ do
    let ctx = setUserData (EmbedStack [1, 2, 3]) emptyRenderCtx
    getUserData @EmbedStack ctx `shouldBe` Just (EmbedStack [1, 2, 3])

  it "returns Nothing when the requested type doesn't match what was stored" $ do
    let ctx = setUserData (EmbedStack [1]) emptyRenderCtx
    getUserData @OtherTag ctx `shouldBe` Nothing

  it "setUserData replaces the previous value (single-slot semantics)" $ do
    let ctx =
          emptyRenderCtx
            & setUserData (EmbedStack [1])
            & setUserData (EmbedStack [2, 3])
    getUserData @EmbedStack ctx `shouldBe` Just (EmbedStack [2, 3])

  it "setUserData of a different type replaces the previous slot" $ do
    let ctx =
          emptyRenderCtx
            & setUserData (EmbedStack [1])
            & setUserData (OtherTag 42)
    -- The earlier 'EmbedStack' is gone; the new 'OtherTag' is what's stored.
    getUserData @EmbedStack ctx `shouldBe` Nothing
    getUserData @OtherTag ctx `shouldBe` Just (OtherTag 42)
  where
    (&) = flip ($)
