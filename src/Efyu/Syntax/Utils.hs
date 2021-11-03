module Efyu.Syntax.Utils where

import Text.Parsec

type EfyuParser u a = Parsec String u a

whitespace :: EfyuParser u String
whitespace = many $ oneOf [' ', '\n', '\t']

commentString :: EfyuParser u String
commentString = whitespace >> string "//" >> anyChar `manyTill` newline

withWhitespace :: EfyuParser u a -> EfyuParser u a
withWhitespace = between whitespace whitespace

withOptionalParens :: EfyuParser u a -> EfyuParser u a
withOptionalParens comb =
  between (char '(') (char ')') comb <|> comb
