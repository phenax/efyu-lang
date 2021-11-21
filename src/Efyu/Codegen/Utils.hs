module Efyu.Codegen.Utils where

import Data.List (intercalate)

class CodeGenerator a where
  codegen :: a -> String

indent = unlines . map (ind ++)
  where
    ind = replicate 2 ' '

between s e x = s ++ x ++ e

parens = between "(" ")"

braces = between "{" "}"

brackets = between "[" "]"

commaList :: (CodeGenerator a) => [a] -> String
commaList = intercalate "," . map codegen
