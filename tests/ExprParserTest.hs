module ExprParserTest where

import Efyu.Syntax.Expression
import Efyu.Syntax.Syntax
import Efyu.Syntax.Type (typeAnnotationP)
import Efyu.Types.Types
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

  describe "comments" $ do
    it "should ignore single line comments" $ do
      p [r|x -- wow|] `shouldParse` Var "x"
      p [r|\x -> y -- wow|] `shouldParse` ("x" *->> Var "y")
      p
        [r|
        let
          x = 1 -- define var
        in x
        |]
        `shouldParse` Let [("x", int 1)] (Var "x")
    it "should ignore multi line comments" $ do
      p [r|x {- comment -}|] `shouldParse` Var "x"
      p
        [r|\x -> y
      {-
  Multiline comments
-}
        |]
        `shouldParse` ("x" *->> Var "y")

  describe "let binding expression" $ do
    it "should parse simple let binding" $ do
      p [r| let x = 200. in x |]
        `shouldParse` Let [("x", float 200.0)] (Var "x")
      p [r| let x a = add a in x |]
        `shouldParse` Let [("x", "a" *->> Var "add" `call` Var "a")] (Var "x")
      p [r| let x a b = add a b in x |]
        `shouldParse` Let [("x", "a" *->> "b" *->> Var "add" `call` Var "a" `call` Var "b")] (Var "x")
    it "should parse nested let binding" $ do
      p [r| let x = let y = 200.0 in y; in x |]
        `shouldParse` Let
          [ ( "x",
              Let
                [("y", float 200.0)]
                (Var "y")
            )
          ]
          (Var "x")
      p -- TODO: This is incorrect behavior fix later
        [r|
        let
          x = let
                y = 200.0
            in y
          in x |]
        `shouldParse` Let
          [ ( "x",
              Let
                [("y", float 200.0)]
                (Var "y")
            )
          ]
          (Var "x")
    it "should parse different layouts of writing let bindings with indents" $ do
      p
        [r| let x = 200. in x |]
        `shouldParse` Let [("x", float 200.0)] (Var "x")
      p
        [r|
          let x = 200. in
            x |]
        `shouldParse` Let [("x", float 200.0)] (Var "x")
      p
        [r| let
            x = 200.
          in x |]
        `shouldParse` Let [("x", float 200.0)] (Var "x")
      p
        [r|
          let x = 200.
          in x
        |]
        `shouldParse` Let [("x", Literal $ LiteralFloat 200.0)] (Var "x")
      p [r| let x = 200.; y = "wow"; in x|]
        `shouldParse` Let
          [ ("x", float 200.0),
            ("y", str "wow")
          ]
          (Var "x")
      p
        [r|
          let
            x = 200.
            y = "wow";
          in x|]
        `shouldParse` Let
          [ ("x", float 200.0),
            ("y", str "wow")
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
          [ ("x", float 200.0),
            ("y", str "wow")
          ]
          (Var "x")
      p
        [r|
          let
            x =
              add 1 2
            y =
              "wow"
          in x|]
        `shouldParse` Let
          [ ("x", Var "add" `call` int 1 `call` int 2),
            ("y", str "wow")
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
      p [r| \x -> 200 |] `shouldParse` ("x" *->> int 200)
      p [r| \x -> "wow" |] `shouldParse` ("x" *->> str "wow")
    it "should parse nested lambdas" $ do
      p [r| \x -> \foobar -> add x foobar |]
        `shouldParse` ("x" *->> "foobar" *->> Var "add" `call` Var "x" `call` Var "foobar")
    it "should parse lambda inside let definition" $ do
      p
        [r|
        let
          fn =
            \x -> mul x 5
         in fn 5
        |]
        `shouldParse` Let
          [("fn", "x" *->> Var "mul" `call` Var "x" `call` int 5)]
          (Var "fn" `call` int 5)
    it "should parse lambda defintion in let" $ do
      p
        [r|
        let
          fn x =
            mul x 5
         in fn 5
        |]
        `shouldParse` Let
          [("fn", "x" *->> Var "mul" `call` Var "x" `call` int 5)]
          (Var "fn" `call` int 5)

  describe "apply expression" $ do
    it "should parse simple application" $ do
      p [r|add 2|] `shouldParse` (Var "add" `call` int 2)
      p [r| add x 2.1 |] `shouldParse` (Var "add" `call` Var "x" `call` float 2.1)
      p [r| (foo x 1) |] `shouldParse` (Var "foo" `call` Var "x" `call` int 1)
      p
        [r|
        add
          2
          2.1 |]
        `shouldParse` (Var "add" `call` int 2 `call` float 2.1)

    it "should parse apply for lambda expression" $ do
      p [r| goob ber |] `shouldParse` (Var "goob" `call` Var "ber")
      p [r| (\x -> x) y |] `shouldParse` (("x" *->> Var "x") `call` Var "y")

    it "should parse nested application" $ do
      p [r| foo (bar x) |]
        `shouldParse` (Var "foo" `call` (Var "bar" `call` Var "x"))
      p [r| add (foo x 1) y |]
        `shouldParse` (Var "add" `call` (Var "foo" `call` Var "x" `call` int 1) `call` Var "y")
      p [r| (add (foo x 1)) y |]
        `shouldParse` (Var "add" `call` (Var "foo" `call` Var "x" `call` int 1) `call` Var "y")
      p [r| ((foo x 1) y) |]
        `shouldParse` ((Var "foo" `call` Var "x" `call` int 1) `call` Var "y")

    it "should parse apply inside let" $ do
      p [r| let x = add 1 in x |]
        `shouldParse` Let [("x", Var "add" `call` int 1)] (Var "x")
      p
        [r|
        let
          x = 1
        in add 1 x |]
        `shouldParse` Let [("x", int 1)] (Var "add" `call` int 1 `call` Var "x")
      p
        [r|
        let
          x = add 1 2 3
        in x|]
        `shouldParse` Let [("x", Var "add" `call` int 1 `call` int 2 `call` int 3)] (Var "x")
      p
        [r|
        let
          x = add 1 2
          y = get "num" 2;
        in mul x y|]
        `shouldParse` Let
          [ ("x", Var "add" `call` int 1 `call` int 2),
            ("y", Var "get" `call` str "num" `call` int 2)
          ]
          (Var "mul" `call` Var "x" `call` Var "y")

    it "should allow different layouts" $ do
      p
        [r|
        add
          x
          y |]
        `shouldParse` (Var "add" `call` Var "x" `call` Var "y")
      p
        [r|
        add
          (foo
            x)
          y |]
        `shouldParse` (Var "add" `call` (Var "foo" `call` Var "x") `call` Var "y")
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
            foo :: Int -> String
            foo x = show x
          in foo 5
        |]
        `shouldParse` Let
          [ ("_", TypeAnnotation "foo" $ TInt `tlam` TString),
            ("foo", "x" *->> Var "show" `call` Var "x")
          ]
          (Var "foo" `call` int 5)
    it "should parse definitions types in muliple lines" $ do
      p
        [r|
        let
            foo :: Int -> String
            foo x = show x
            foo1 ::
              Int ->
              Int
            foo1 x = 1
          in foo 5
        |]
        `shouldParse` Let
          [ ("_", TypeAnnotation "foo" $ TInt `tlam` TString),
            ("foo", "x" *->> Var "show" `call` Var "x"),
            ("_", TypeAnnotation "foo1" $ TInt `tlam` TInt),
            ("foo1", "x" *->> int 1)
          ]
          (Var "foo" `call` int 5)

  describe "typeAnnotationP > type annotations" $ do
    let tp = MP.parse (typeAnnotationP <* eof) "type.fu"
    it "should parse primitive types" $ do
      tp [r|name :: String |] `shouldParse` TypeAnnotation "name" TString
      tp [r|name :: Int |] `shouldParse` TypeAnnotation "name" TInt
      tp [r|name :: Bool |] `shouldParse` TypeAnnotation "name" TBool
      tp [r|name :: Float|] `shouldParse` TypeAnnotation "name" TFloat
    it "should parse lambda types" $ do
      tp [r|fn :: String -> Int |] `shouldParse` TypeAnnotation "fn" (TString `tlam` TInt)
      tp [r|fn :: Bool -> String -> Int |] `shouldParse` TypeAnnotation "fn" (TBool `tlam` TString `tlam` TInt)
      tp [r|fn :: a -> b -> c |] `shouldParse` TypeAnnotation "fn" (TVar "a" `tlam` TVar "b" `tlam` TVar "c")

  describe "if-then-else" $ do
    it "should parse if expressions" $ do
      p [r| if x then f x else g x |]
        `shouldParse` IfElse
          (Var "x")
          (Var "f" `call` Var "x")
          (Var "g" `call` Var "x")
      p [r| if cond x 0 then f x 1 else g x 2 |]
        `shouldParse` IfElse
          (Var "cond" `call` Var "x" `call` int 0)
          (Var "f" `call` Var "x" `call` int 1)
          (Var "g" `call` Var "x" `call` int 2)
      p
        [r|
          if cond x 0 then
            f x 1
          else
            g x 2
        |]
        `shouldParse` IfElse
          (Var "cond" `call` Var "x" `call` int 0)
          (Var "f" `call` Var "x" `call` int 1)
          (Var "g" `call` Var "x" `call` int 2)
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
          [ ( "cond",
              IfElse
                (Var "test")
                (Var "f" `call` int 1)
                (Var "g" `call` int 2)
            )
          ]
          (IfElse (Var "cond" `call` int 1) (int 1) (int 2))

--
--
--
--
--
--
--
