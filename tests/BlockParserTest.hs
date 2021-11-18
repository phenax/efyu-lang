module BlockParserTest where

import Efyu.Syntax.Block
import Efyu.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import TestHelpers
import Text.Megaparsec (MonadParsec (eof))
import qualified Text.Megaparsec as MP
import Text.RawString.QQ (r)

tests = do
  let parse = MP.parse (blockP "Main" <* eof) "testfile.fu"

  describe "define block statement" $ do
    it "should parse definitions into a main module" $ do
      parse [r|num = 20|]
        `shouldParse` Module "Main" [Def $ defVal "num" (int 20)]
      parse
        [r|
num = 20 |]
        `shouldParse` Module "Main" [Def $ defVal "num" (int 20)]
      parse
        `shouldFailOn` [r|
num =
20 |]
      parse
        [r|
num =
  let
    x = 2
  in x |]
        `shouldParse` Module "Main" [Def $ defVal "num" (Let [defVal "x" (int 2)] (var "x"))]
      parse
        `shouldFailOn` [r|
                 num = 20 |]
    it "should parse multiple definitions" $ do
      parse
        [r|
num = 20
str = "ww"
fn a b c = stuff b c a |]
        `shouldParse` Module
          "Main"
          [ Def $ defVal "num" (int 20),
            Def $ defVal "str" (str "ww"),
            Def $ defVal "fn" $ "a" *->> "b" *->> "c" *->> var "stuff" `call` var "b" `call` var "c" `call` var "a"
          ]
    it "should parse different styles for definitions" $ do
      parse
        [r|
num =
  20

str =
  "ww"

fn a b =
  stuff
    a
    b

dummy = 2

|]
        `shouldParse` Module
          "Main"
          [ Def $ defVal "num" (int 20),
            Def $ defVal "str" (str "ww"),
            Def $ defVal "fn" $ "a" *->> "b" *->> var "stuff" `call` var "a" `call` var "b",
            Def $ defVal "dummy" (int 2)
          ]

  describe "definitions with type annotations" $ do
    it "should parse definitions along with annotations" $ do
      parse
        [r|
num : Int
num = 20
|]
        `shouldParse` Module
          "Main"
          [ Def $ defSig "num" TInt,
            Def $ defVal "num" (int 20)
          ]
      parse
        [r|
ello : Int
ello = 20

num : Int -> String -> Bool
num a b = c
|]
        `shouldParse` Module
          "Main"
          [ Def $ defSig "ello" TInt,
            Def $ defVal "ello" (int 20),
            Def $ defSig "num" $ TInt `tlam` TString `tlam` TBool,
            Def $ defVal "num" ("a" *->> "b" *->> var "c")
          ]

  describe "custom type alias def" $ do
    it "should parse definitions along with annotations" $ do
      parse
        [r|
type DummyInt = Int
foobar : DummyInt
|]
        `shouldParse` Module
          "Main"
          [ TypeDef (IdentifierName "DummyInt") TInt,
            Def $ defSig "foobar" (tname "DummyInt")
          ]
    it "should parse type scoped definitions" $ do
      parse
        [r|
type MyPair a b = (a, b)
|]
        `shouldParse` Module
          "Main"
          [ TypeDef (IdentifierName "MyPair") $
              ident "a" `TScope` (ident "b" `TScope` TTuple [tvar "a", tvar "b"])
          ]

    it "should parse complex type aliases" $ do
      parse
        [r|
type MyType1 = (Int, a, (b, String), Fancy (Maybe Int) Float)
type MyType2 = Int -> [Int] -> Maybe Int
type MyType3 = SomeType a String Float
type MyType4 = [[(Int, a -> b)]]

|]
        `shouldParse` Module
          "Main"
          [ TypeDef (IdentifierName "MyType1") $
              TTuple
                [ TInt,
                  tvar "a",
                  TTuple [tvar "b", TString],
                  tname "Fancy" `TApply` (tname "Maybe" `TApply` TInt) `TApply` TFloat
                ],
            TypeDef (IdentifierName "MyType2") $
              TInt `tlam` TList TInt `tlam` (tname "Maybe" `TApply` TInt),
            TypeDef (IdentifierName "MyType3") $
              tname "SomeType" `TApply` tvar "a" `TApply` TString `TApply` TFloat,
            TypeDef (IdentifierName "MyType4") $
              TList $ TList $ TTuple [TInt, tvar "a" `tlam` tvar "b"]
          ]

---
