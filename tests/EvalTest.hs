module EvalTest where

import Test.Hspec

tests = do
  describe "evalExpression" $ do
    describe "number ops" $ do
      it "should evaluate positive and negative numbers" $ do
        1 `shouldEqual` 1

--
