module BlockParserTest where

import Efyu.Syntax.Block
import Efyu.Syntax.Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (MonadParsec (eof))
import qualified Text.Megaparsec as MP
import Text.RawString.QQ (r)

tests = do
  let parse = MP.parse (blockP "Main" <* eof) "testfile.fu"

  describe "define block statement" $ do
    it "should parse definitions into a main module" $ do
      parse [r|num = 20|]
        `shouldParse` Module "Main" [Def "num" (Literal $ LiteralInt 20)]
      parse
        [r|
num = 20 |]
        `shouldParse` Module "Main" [Def "num" (Literal $ LiteralInt 20)]
      parse
        [r|
num =
  let
    x = 2
  in x |]
        `shouldParse` Module "Main" [Def "num" (Let [("x", Literal $ LiteralInt 2)] (Var "x"))]
      parse
        `shouldFailOn` [r|
                 num = 20 |]
    it "ww" $ do
      parse
        [r|
num = 20

str = "2" |]
        `shouldParse` Module "Main" [Def "num" (Literal $ LiteralInt 20)]
