module ExprParserTest where

import Efyu.Syntax.Expression
import Efyu.Syntax.Utils (MParser, parser, scnl)
import Efyu.Types
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
      p [r|"121lk~!@#$%^&*()_+-=`[]\\;',./"|]
        `shouldParse` Literal (LiteralString "121lk~!@#$%^&*()_+-=`[]\\;',./")
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
    it "should parse tuple expressions" $ do
      p [r|(1)|] `shouldParse` int 1 -- Single item isn't a tuple
      p [r|( 1,  2,3   ,4 )|] `shouldParse` tuple [int 1, int 2, int 3, int 4]
      p [r|( 1, "ww" ,4 )|] `shouldParse` tuple [int 1, str "ww", int 4]
      p [r|foobar 1 (x, y) 2|]
        `shouldParse` (var "foobar" `call` int 1 `call` tuple [var "x", var "y"] `call` int 2)
      p
        [r|
          (
            foo,
            bar,
          )
        |]
        `shouldParse` tuple [var "foo", var "bar"]
      p
        [r|
          (
            let
              x = 1
            in x,
            5,
          )
        |]
        `shouldParse` tuple [Let [defVal "x" $ int 1] (var "x"), int 5]
      p
        `shouldFailOn` [r|
          (
            foo,
          bar,
          )
        |]
    it "should parse literal list expressions" $ do
      p [r|[1]|] `shouldParse` Literal (LiteralList [int 1])
      p [r|[ 1,  2,3   ,4 ]|] `shouldParse` Literal (LiteralList [int 1, int 2, int 3, int 4])
      p [r|[ 1, "ww" ,4 ]|] `shouldParse` Literal (LiteralList [int 1, str "ww", int 4])
      p
        [r|
          [
            foo,
            bar,
          ]
        |]
        `shouldParse` Literal (LiteralList [var "foo", var "bar"])
      p
        [r|
          [
            let
              x = 1
            in x,
            5,
          ]
        |]
        `shouldParse` Literal (LiteralList [Let [defVal "x" $ int 1] (var "x"), int 5])
      p
        `shouldFailOn` [r|
          [
            foo,
          bar,
          ]
        |]

  describe "variables" $ do
    it "should parse alphanumeric identifiers" $ do
      p [r|foobar|] `shouldParse` var "foobar"
      p [r|foobar121|] `shouldParse` var "foobar121"
      p [r|foo'x|] `shouldParse` var "foo'x"
      p [r|foo?x|] `shouldParse` var "foo?x"
      p [r|foo_bar|] `shouldParse` var "foo_bar"
      p "\n  foo_bar " `shouldParse` var "foo_bar"
      p `shouldFailOn` "121foobar"
      p `shouldFailOn` "''1foobar"
      p `shouldFailOn` "?foobar"
      p `shouldFailOn` "$1foobar"
      p `shouldFailOn` "\"1foobar"

  describe "comments" $ do
    it "should ignore single line comments" $ do
      p [r|x -- wow|] `shouldParse` var "x"
      p [r|\x -> y -- wow|] `shouldParse` ("x" *->> var "y")
      p
        [r|
        let
          x = 1 -- define var
        in x
        |]
        `shouldParse` Let [defVal "x" (int 1)] (var "x")
    it "should ignore multi line comments" $ do
      p [r|x {- comment -}|] `shouldParse` var "x"
      p
        [r|\x -> y
      {-
  Multiline comments
-}
        |]
        `shouldParse` ("x" *->> var "y")

  describe "let binding expression" $ do
    it "should parse simple let binding" $ do
      p [r| let x = 200. in x |]
        `shouldParse` Let [defVal "x" (float 200.0)] (var "x")
      p [r| let x a = add a in x |]
        `shouldParse` Let [defVal "x" ("a" *->> var "add" `call` var "a")] (var "x")
      p [r| let x a b = add a b in x |]
        `shouldParse` Let [defVal "x" ("a" *->> "b" *->> var "add" `call` var "a" `call` var "b")] (var "x")
    it "should parse nested let binding" $ do
      p [r| let x = let y = 200.0 in y; in x |]
        `shouldParse` Let
          [ defVal "x" $
              Let
                [defVal "y" (float 200.0)]
                (var "y")
          ]
          (var "x")
      p
        [r|
        let
          x =
            let
              y = 200.0
            in y
        in x |]
        `shouldParse` Let
          [ defVal "x" $
              Let
                [defVal "y" (float 200.0)]
                (var "y")
          ]
          (var "x")
      p
        [r|
        let
          x = let
                y = 200.0
              in y
        in x |]
        `shouldParse` Let
          [ defVal "x" $
              Let
                [defVal "y" (float 200.0)]
                (var "y")
          ]
          (var "x")
    it "should parse different layouts of writing let bindings with indents" $ do
      p
        [r| let x = 200. in x |]
        `shouldParse` Let [defVal "x" (float 200.0)] (var "x")
      p
        [r|
          let x = 200. in
            x |]
        `shouldParse` Let [defVal "x" (float 200.0)] (var "x")
      p
        [r| let
            x = 200.
          in x |]
        `shouldParse` Let [defVal "x" (float 200.0)] (var "x")
      p
        [r|
          let x = 200.
          in x
        |]
        `shouldParse` Let [defVal "x" (Literal $ LiteralFloat 200.0)] (var "x")
      p [r| let x = 200.; y = "wow"; in x|]
        `shouldParse` Let
          [ defVal "x" (float 200.0),
            defVal "y" (str "wow")
          ]
          (var "x")
      p
        [r|
          let
            x = 200.
            y = "wow";
          in x|]
        `shouldParse` Let
          [ defVal "x" (float 200.0),
            defVal "y" (str "wow")
          ]
          (var "x")
      p
        [r|
          let
            x =
              200.
            y =
              "wow"
          in x|]
        `shouldParse` Let
          [ defVal "x" (float 200.0),
            defVal "y" (str "wow")
          ]
          (var "x")
      p
        [r|
          let
            x =
              add 1 2
            y =
              "wow"
          in x|]
        `shouldParse` Let
          [ defVal "x" (var "add" `call` int 1 `call` int 2),
            defVal "y" (str "wow")
          ]
          (var "x")
      p
        `shouldFailOn` [r|
          let
          x = 200.;
          in x |]
      p
        `shouldFailOn` [r|
          let x = 200.; in
          x |]
      p
        `shouldFailOn` [r|
          let
            x = 200.
        in x |]

  describe "lambda expression" $ do
    it "should parse simple lambda" $ do
      p [r|\x -> x|] `shouldParse` ("x" *->> var "x")
      p [r| \x -> 200 |] `shouldParse` ("x" *->> int 200)
      p [r| \x -> "wow" |] `shouldParse` ("x" *->> str "wow")
    it "should parse nested lambdas" $ do
      p [r| \x -> \foobar -> add x foobar |]
        `shouldParse` ("x" *->> "foobar" *->> var "add" `call` var "x" `call` var "foobar")
    it "should parse lambda inside let definition" $ do
      p
        [r|
        let
          fn =
            \x -> mul x 5
        in fn 5
        |]
        `shouldParse` Let
          [defVal "fn" $ "x" *->> var "mul" `call` var "x" `call` int 5]
          (var "fn" `call` int 5)
    it "should parse lambda defintion in let" $ do
      p
        [r|
        let
          fn x =
            mul x 5
        in fn 5
        |]
        `shouldParse` Let
          [defVal "fn" $ "x" *->> var "mul" `call` var "x" `call` int 5]
          (var "fn" `call` int 5)

  describe "apply expression" $ do
    it "should parse simple application" $ do
      p [r|add 2|] `shouldParse` (var "add" `call` int 2)
      p [r| add x 2.1 |] `shouldParse` (var "add" `call` var "x" `call` float 2.1)
      p [r| (foo x 1) |] `shouldParse` (var "foo" `call` var "x" `call` int 1)
      p
        [r|
        add
          2
          2.1 |]
        `shouldParse` (var "add" `call` int 2 `call` float 2.1)

    it "should parse apply for lambda expression" $ do
      p [r| goob ber |] `shouldParse` (var "goob" `call` var "ber")
      p [r| (\x -> x) y |] `shouldParse` (("x" *->> var "x") `call` var "y")

    it "should parse nested application" $ do
      p [r| foo (bar x) |]
        `shouldParse` (var "foo" `call` (var "bar" `call` var "x"))
      p [r| add (foo x 1) y |]
        `shouldParse` (var "add" `call` (var "foo" `call` var "x" `call` int 1) `call` var "y")
      p [r| (add (foo x 1)) y |]
        `shouldParse` (var "add" `call` (var "foo" `call` var "x" `call` int 1) `call` var "y")
      p [r| ((foo x 1) y) |]
        `shouldParse` ((var "foo" `call` var "x" `call` int 1) `call` var "y")

    it "should parse apply inside let" $ do
      p [r| let x = add 1 in x |]
        `shouldParse` Let [defVal "x" $ var "add" `call` int 1] (var "x")
      p
        [r|
        let
          x = 1
        in add 1 x |]
        `shouldParse` Let [defVal "x" (int 1)] (var "add" `call` int 1 `call` var "x")
      p
        [r|
        let
          x = add 1 2 3
        in x|]
        `shouldParse` Let [defVal "x" $ var "add" `call` int 1 `call` int 2 `call` int 3] (var "x")
      p
        [r|
        let
          x = add 1 2
          y = get "num" 2;
        in mul x y|]
        `shouldParse` Let
          [ defVal "x" (var "add" `call` int 1 `call` int 2),
            defVal "y" (var "get" `call` str "num" `call` int 2)
          ]
          (var "mul" `call` var "x" `call` var "y")

    it "should allow different layouts" $ do
      p
        [r|
        add
          x
          y |]
        `shouldParse` (var "add" `call` var "x" `call` var "y")
      p
        [r|
        add
          (foo
            x)
          y |]
        `shouldParse` (var "add" `call` (var "foo" `call` var "x") `call` var "y")
      p
        `shouldFailOn` [r|
        add
        (foo
        x)
        y |]
      p
        `shouldFailOn` [r|
        add (foo
        x) y |]

  describe "type annotations in let defintion" $ do
    it "should parse definitions types" $ do
      p
        [r|
        let
            foo : Int -> String
            foo x = show x
          in foo 5
        |]
        `shouldParse` Let
          [ defSig "foo" (TInt `tlam` TString),
            defVal "foo" ("x" *->> var "show" `call` var "x")
          ]
          (var "foo" `call` int 5)
    it "should parse definitions types in muliple lines" $ do
      p
        [r|
        let
            foo : Int -> String
            foo x = show x
            foo1 :
              Int ->
              Int
            foo1 x = 1
          in foo 5
        |]
        `shouldParse` Let
          [ defSig "foo" (TInt `tlam` TString),
            defVal "foo" ("x" *->> var "show" `call` var "x"),
            defSig "foo1" (TInt `tlam` TInt),
            defVal "foo1" ("x" *->> int 1)
          ]
          (var "foo" `call` int 5)

  describe "definition > type annotations" $ do
    let tp = MP.parse ((parser scnl :: MParser Definition) <* eof) "type.fu"
    it "should parse primitive types" $ do
      tp [r|name : String |] `shouldParse` defSig "name" TString
      tp [r|name : Int |] `shouldParse` defSig "name" TInt
      tp [r|name : Bool |] `shouldParse` defSig "name" TBool
      tp [r|name : Float|] `shouldParse` defSig "name" TFloat
      tp [r|name : (Int, Float)|] `shouldParse` defSig "name" (TTuple [TInt, TFloat])
    it "should parse lambda types" $ do
      tp [r|fn : String -> Int |] `shouldParse` defSig "fn" (TString `tlam` TInt)
      tp [r|fn: Bool -> String -> Int |] `shouldParse` defSig "fn" (TBool `tlam` TString `tlam` TInt)
      tp [r|fn : (Bool -> String) -> Int |] `shouldParse` defSig "fn" ((TBool `tlam` TString) `tlam` TInt)
      tp [r|fn : Bool -> (String -> Int) |] `shouldParse` defSig "fn" (TBool `tlam` TString `tlam` TInt)
      tp [r|fn : Bool -> ((String) -> Int) |] `shouldParse` defSig "fn" (TBool `tlam` TString `tlam` TInt)
      tp [r|fn : Bool -> ((a -> b) -> Int) |]
        `shouldParse` defSig "fn" (TBool `tlam` (tvar "a" `tlam` tvar "b") `tlam` TInt)
      tp [r|fn: a -> b -> c |] `shouldParse` defSig "fn" (tvar "a" `tlam` tvar "b" `tlam` tvar "c")
      tp [r|fn : a -> (Int, Float)|] `shouldParse` defSig "fn" (tvar "a" `tlam` TTuple [TInt, TFloat])
      tp [r|fn : (Int, Float) -> b|] `shouldParse` defSig "fn" (TTuple [TInt, TFloat] `tlam` tvar "b")

  describe "if-then-else" $ do
    it "should parse if expressions" $ do
      p [r| if x then f x else g x |]
        `shouldParse` IfElse
          (var "x")
          (var "f" `call` var "x")
          (var "g" `call` var "x")
      p [r| if cond x 0 then f x 1 else g x 2 |]
        `shouldParse` IfElse
          (var "cond" `call` var "x" `call` int 0)
          (var "f" `call` var "x" `call` int 1)
          (var "g" `call` var "x" `call` int 2)
      p
        [r|
          if cond x 0 then
            f x 1
          else
            g x 2
        |]
        `shouldParse` IfElse
          (var "cond" `call` var "x" `call` int 0)
          (var "f" `call` var "x" `call` int 1)
          (var "g" `call` var "x" `call` int 2)
      p
        [r|
          let
            cond = if test then
                    f 1
                  else
                    g 2
            in
              if cond 1 then
                1
              else 2
        |]
        `shouldParse` Let
          [ defVal "cond" $
              IfElse
                (var "test")
                (var "f" `call` int 1)
                (var "g" `call` int 2)
          ]
          (IfElse (var "cond" `call` int 1) (int 1) (int 2))

--
--
--
--
--
--
--
