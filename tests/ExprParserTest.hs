module ExprParserTest where

import Data.List (intercalate)
import Efyu.Syntax.Expr (exprParser)
import Efyu.Syntax.Expression
import Efyu.Syntax.Syntax
import Test.Hspec
import qualified Text.Megaparsec as MP
import Text.Parsec (parse)
import Text.RawString.QQ (r)

tests = do
  let p = parse parseExpression "Expr"
  let pp = MP.parse exprParser "wow"

  describe "wow" $ do
    fit "foobar" $ do
      pp
        ( intercalate
            "\n"
            [ "",
              "foobar",
              "  wow",
              "    nnice",
              "      cool",
              "    1nnice1",
              "      1cool1",
              "  cooltl",
              "  gogogoo",
              ""
            ]
        )
        `shouldBe` Right ("", [])

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

  describe "variables" $ do
    it "should parse alphanumeric identifiers" $ do
      p [r|foobar|] `shouldBe` Right (Var "foobar")
      p [r|foobar121|] `shouldBe` Right (Var "foobar121")
  -- p [r|121foobar|] `shouldBe` Right (Var "x") -- TODO: should error out

  describe "let binding expression" $ do
    it "should parse let bindings" $ do
      p
        [r|
          let
            x = 200.;
            y = "wow";
          in x|]
        `shouldBe` Right
          ( Let
              [ ("x", Literal $ LiteralFloat 200.0),
                ("y", Literal $ LiteralString "wow")
              ]
              $ Var "x"
          )

  describe "lambda expression" $ do
    it "should parse simple lambda" $ do
      p [r|\x -> x|] `shouldBe` Right (Lambda "x" (Var "x"))

    -- p [r| \x -> 200 |] `shouldBe` Right (Lambda "x" (Literal . LiteralInt $ 200))
    -- p [r| \x -> "wow" |] `shouldBe` Right (Lambda "x" (Literal . LiteralString $ "wow"))
    xit "should parse lambda" $ do
      p [r| \x -> \foobar -> add x foobar |]
        `shouldBe` Right
          ( Lambda
              "x"
              ( Lambda
                  "foobar"
                  ( Apply
                      (Apply (Var "add") (Var "x"))
                      (Var "y")
                  )
              )
          )

--
