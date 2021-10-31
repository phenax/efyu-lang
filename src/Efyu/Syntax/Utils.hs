module Efyu.Syntax.Utils where

import Text.Parsec

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

commentString :: Parsec String u String
commentString = whitespace >> string "//" >> anyChar `manyTill` newline

withWhitespace :: Parsec String u a -> Parsec String u a
withWhitespace comb = do
  whitespace
  content <- comb
  whitespace
  return content
