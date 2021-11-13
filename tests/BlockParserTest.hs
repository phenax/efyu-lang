module BlockParserTest where

import Efyu.Syntax.Block
import Efyu.Syntax.Syntax
import Efyu.Types.Types
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
        `shouldParse` Module "Main" [Def $ DefValue "num" (int 20)]
      parse
        [r|
num = 20 |]
        `shouldParse` Module "Main" [Def $ DefValue "num" (int 20)]
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
        `shouldParse` Module "Main" [Def $ DefValue "num" (Let [DefValue "x" (int 2)] (Var "x"))]
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
          [ Def $ DefValue "num" (int 20),
            Def $ DefValue "str" (str "ww"),
            Def $ DefValue "fn" $ "a" *->> "b" *->> "c" *->> Var "stuff" `call` Var "b" `call` Var "c" `call` Var "a"
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
          [ Def $ DefValue "num" (int 20),
            Def $ DefValue "str" (str "ww"),
            Def $ DefValue "fn" $ "a" *->> "b" *->> Var "stuff" `call` Var "a" `call` Var "b",
            Def $ DefValue "dummy" (int 2)
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
          [ Def $ DefSignature "num" TInt,
            Def $ DefValue "num" (int 20)
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
          [ Def $ DefSignature "ello" TInt,
            Def $ DefValue "ello" (int 20),
            Def $ DefSignature "num" $ TInt `tlam` TString `tlam` TBool,
            Def $ DefValue "num" ("a" *->> "b" *->> Var "c")
          ]
