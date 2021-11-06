module Main where

import Efyu.Syntax.Parse (parseFile)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple (pPrint)

main = do
  ast <- parseFile "./examples/scratchpad.fu"
  -- typecheck
  case ast of
    Left e -> putStrLn . errorBundlePretty $ e
    Right r -> pPrint r
