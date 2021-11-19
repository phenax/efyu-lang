module Main where

import Efyu (typeCheckFile)
import Text.Pretty.Simple (pPrint)

main = do
  block <- typeCheckFile "./examples/scratchpad.fu"
  case block of
    Left e -> print e
    Right r -> pPrint r
