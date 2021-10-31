module TypeTest where

import Test.Hspec

tests =
  describe "Type inference" $ do
    it "should do stuff" $ do
      1 `shouldBe` 1
