module Main where

import Efyu.Syntax.Parse (parseFile)
import Text.Pretty.Simple (pPrint)

main = do
  ast <- parseFile "./examples/scratchpad.fu"
  -- typecheck
  pPrint ast
