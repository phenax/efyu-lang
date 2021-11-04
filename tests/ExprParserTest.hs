module ExprParserTest where

import Efyu.Syntax.Expression
import Efyu.Syntax.Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelpers
import Text.Megaparsec (MonadParsec (eof))
import qualified Text.Megaparsec as MP
import Text.RawString.QQ (r)

tests = do
  let p = MP.parse (expressionP <* eof) "testfile.fu"

  describe "literal expression" $ do
    it "should parse literal string" $ do
      p [r|"hell"|] `shouldParse` Literal (LiteralString "hell")
      p [r|"121lk~!@#$%^&*()_+-=`[]\\;',./"|] `shouldParse` Literal (LiteralString "121lk~!@#$%^&*()_+-=`[]\\;',./")
    it "should parse literal int" $ do
      p [r|2005|] `shouldParse` Literal (LiteralInt 2005)
      p [r|0|] `shouldParse` Literal (LiteralInt 0)
      p [r|-2005|] `shouldParse` Literal (LiteralInt (-2005))
    it "should parse literal bool" $ do
      p [r|True|] `shouldParse` Literal (LiteralBool True)
      p [r|False|] `shouldParse` Literal (LiteralBool False)
    it "should parse literal float" $ do
      p [r|20.05|] `shouldParse` Literal (LiteralFloat 20.05)
      p [r|20.|] `shouldParse` Literal (LiteralFloat 20.0)
      p [r|-20.05|] `shouldParse` Literal (LiteralFloat (-20.05))
      p [r|-20.|] `shouldParse` Literal (LiteralFloat (-20.0))

  describe "variables" $ do
    it "should parse alphanumeric identifiers" $ do
      p [r|foobar|] `shouldParse` Var "foobar"
      p [r|foobar121|] `shouldParse` Var "foobar121"
      p [r|foo'x|] `shouldParse` Var "foo'x"
      p [r|foo?x|] `shouldParse` Var "foo?x"
      p [r|foo_bar|] `shouldParse` Var "foo_bar"
      p "\n  foo_bar " `shouldParse` Var "foo_bar"
      p `shouldFailOn` "121foobar"
      p `shouldFailOn` "''1foobar"
      p `shouldFailOn` "?foobar"
      p `shouldFailOn` "$1foobar"
      p `shouldFailOn` "\"1foobar"

  describe "let binding expression" $ do
    it "should parse simple let binding" $ do
      p [r| let x = 200. in x |]
        `shouldParse` Let [("x", Literal $ LiteralFloat 200.0)] (Var "x")
    it "should parse nested let binding" $ do
      p [r| let x = let y = 200.0 in y; in x |]
        `shouldParse` Let
          [ ( "x",
              Let
                [("y", Literal $ LiteralFloat 200.0)]
                (Var "y")
            )
          ]
          (Var "x")
      p -- TODO: This is incorrect behavior fix later
        [r|
        let x =
              let y = 200.0
            in y;
          in x |]
        `shouldParse` Let
          [ ( "x",
              Let
                [("y", Literal $ LiteralFloat 200.0)]
                (Var "y")
            )
          ]
          (Var "x")
    it "should parse different layouts of writing let bindings with indents" $ do
      p
        [r| let x = 200. in x |]
        `shouldParse` Let [("x", Literal $ LiteralFloat 200.0)] (Var "x")
      p
        [r|
          let x = 200. in
            x |]
        `shouldParse` Let [("x", Literal $ LiteralFloat 200.0)] (Var "x")
      p
        [r| let
            x = 200.
          in x |]
        `shouldParse` Let [("x", Literal $ LiteralFloat 200.0)] (Var "x")
      p
        [r|
          let x = 200.
          in x
        |]
        `shouldParse` Let [("x", Literal $ LiteralFloat 200.0)] (Var "x")
      p [r| let x = 200.; y = "wow"; in x|]
        `shouldParse` Let
          [ ("x", Literal $ LiteralFloat 200.0),
            ("y", Literal $ LiteralString "wow")
          ]
          (Var "x")
      p
        [r|
          let
            x = 200.
            y = "wow";
          in x|]
        `shouldParse` Let
          [ ("x", Literal $ LiteralFloat 200.0),
            ("y", Literal $ LiteralString "wow")
          ]
          (Var "x")
      p
        [r|
          let
            x =
              200.
            y =
              "wow"
          in x|]
        `shouldParse` Let
          [ ("x", Literal $ LiteralFloat 200.0),
            ("y", Literal $ LiteralString "wow")
          ]
          (Var "x")
      p
        `shouldFailOn` [r|
          let
          x = 200.;
          in x |]
      p
        `shouldFailOn` [r|
          let x = 200.; in
          x |]

    describe "lambda expression" $ do
      it "should parse simple lambda" $ do
        p [r|\x -> x|] `shouldParse` ("x" *->> Var "x")
        p [r| \x -> 200 |] `shouldParse` ("x" *->> Literal (LiteralInt 200))
        p [r| \x -> "wow" |] `shouldParse` ("x" *->> Literal (LiteralString "wow"))
      it "should parse nested lambdas" $ do
        p [r| \x -> \foobar -> @add x foobar |]
          `shouldParse` ( "x" *->> "foobar"
                            *->> Var "add" `call` Var "x" `call` Var "foobar"
                        )

    describe "apply expression" $ do
      it "should parse simple application" $ do
        p [r|@add 2|] `shouldParse` (Var "add" `call` Literal (LiteralInt 2))
        p [r| @add x 2.1 |] `shouldParse` (Var "add" `call` Var "x" `call` Literal (LiteralFloat 2.1))
        p
          [r|
          @add
            2
            2.1 |]
          `shouldParse` (Var "add" `call` Literal (LiteralInt 2) `call` Literal (LiteralFloat 2.1))
      it "should parse apply for lambda expression" $ do
        p [r| @(\x -> x) y |] `shouldParse` (("x" *->> Var "x") `call` Var "y")
      it "should parse nested application" $ do
        p [r| @add (@foo x) y |]
          `shouldParse` (Var "add" `call` (Var "foo" `call` Var "x") `call` Var "y")
        p [r| @(@add (@foo x)) y |]
          `shouldParse` (Var "add" `call` (Var "foo" `call` Var "x") `call` Var "y")
      it "should allow different layouts" $ do
        p
          [r|
          @add
            x
            y |]
          `shouldParse` (Var "add" `call` Var "x" `call` Var "y")
        p
          [r|
          @add
            (@foo
              x)
            y |]
          `shouldParse` (Var "add" `call` (Var "foo" `call` Var "x") `call` Var "y")
        p
          `shouldFailOn` [r|
          @add
          (@foo
          x)
          y |]
        p
          `shouldFailOn` [r|
          @add (@foo
          x) y |]

--
--
--
--
--
--
--
