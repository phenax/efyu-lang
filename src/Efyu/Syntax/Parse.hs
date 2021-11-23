module Efyu.Syntax.Parse where

import Data.Void (Void)
import Efyu.Syntax.Module (moduleP)
import Efyu.Types
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Error (ParseErrorBundle)

type ParseResult = Either (ParseErrorBundle String Void) Module

parse :: String -> String -> ParseResult
parse = MP.parse (moduleP "Main" <* MP.eof)

parseFile :: String -> IO ParseResult
parseFile fp = parse fp <$> readFile fp
