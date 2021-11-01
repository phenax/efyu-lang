module Efyu.Syntax.Utils where

import Text.Parsec

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

commentString :: Parsec String u String
commentString = whitespace >> string "//" >> anyChar `manyTill` newline

withWhitespace :: Parsec String u a -> Parsec String u a
withWhitespace = between whitespace whitespace

withParens :: Parsec String u a -> Parsec String u a
withParens comb =
  between (char '(') (char ')') comb <|> comb
