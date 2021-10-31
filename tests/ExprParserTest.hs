module ExprParserTest where

import Efyu.Syntax.Expression
import Test.Hspec
import Text.Parsec (parse)
import Text.RawString.QQ (r)

tests = do
  let p = parse parseExpression "Expr"

  describe "literal expression" $ do
    it "should parse literal string" $ do
      p [r|"hell"|] `shouldBe` Right (Literal (LiteralString "hell"))
      p [r|"121lk~!@#$%^&*()_+-=`[]\\;',./"|] `shouldBe` Right (Literal (LiteralString "121lk~!@#$%^&*()_+-=`[]\\\\;',./"))
    it "should parse literal int" $ do
      p [r|2005|] `shouldBe` Right (Literal (LiteralInt 2005))
      p [r|0|] `shouldBe` Right (Literal (LiteralInt 0))
      p [r|-2005|] `shouldBe` Right (Literal (LiteralInt (-2005)))
    it "should parse literal bool" $ do
      p [r|True|] `shouldBe` Right (Literal (LiteralBool True))
      p [r|False|] `shouldBe` Right (Literal (LiteralBool False))
    it "should parse literal float" $ do
      p [r|20.05|] `shouldBe` Right (Literal (LiteralFloat 20.05))
      p [r|20.|] `shouldBe` Right (Literal (LiteralFloat 20.0))
      p [r|-20.05|] `shouldBe` Right (Literal (LiteralFloat (-20.05)))
      p [r|-20.|] `shouldBe` Right (Literal (LiteralFloat (-20.0)))

--
