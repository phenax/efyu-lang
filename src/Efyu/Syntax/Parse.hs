module Efyu.Syntax.Parse where

import Data.Void (Void)
import Efyu.Syntax.Block (Block, blockP)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (ParseErrorBundle)

type ParseResult = Either (ParseErrorBundle String Void) Block

parse :: String -> String -> ParseResult
parse = MP.parse (blockP "Main" <* MP.eof)

parseFile :: String -> IO ParseResult
parseFile fp = parse fp <$> readFile fp
