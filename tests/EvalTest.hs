module EvalTest where

import Efyu.Syntax.Parse (parse)
import Test.Hspec

tests = do
  describe "evalExpression" $ do
    describe "number ops" $ do
      it "should evaluate positive and negative numbers" $ do
        parse "wow" `shouldBe` "wow"

--
