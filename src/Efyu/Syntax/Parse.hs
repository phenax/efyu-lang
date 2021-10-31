module Efyu.Syntax.Parse where

import Text.Parsec

data Literal
  = LiteralString String
  | LiteralInt Int
  | LiteralFloat Float
  | LiteralBool Bool
  deriving (Show, Eq)

type Identifier = String

data Expression
  = Literal Literal
  | Let [(Identifier, Expression)] Expression
  | Apply Identifier [Expression]
  deriving (Show, Eq)

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

stringLitP :: Parsec String u Literal
stringLitP = do
  char '"'
  LiteralString <$> anyChar `manyTill` char '"'

intLitP :: Parsec String u Literal
intLitP =
  LiteralInt . read <$> do
    sign <- option "" $ string "-"
    d <- many1 digit
    return $ sign ++ d

floatLitP :: Parsec String u Literal
floatLitP =
  LiteralFloat . read <$> do
    sign <- option "" $ string "-"
    n <- many1 digit
    char '.'
    dec <- many digit
    let decimal = if dec == "" then "0" else dec
    pure $ sign ++ n ++ "." ++ decimal

boolLitP :: Parsec String u Literal
boolLitP = LiteralBool . toBool <$> (string "True" <|> string "False")
  where
    toBool = (== "True")

literalP :: Parsec String u Expression
literalP = Literal <$> literal
  where
    literal =
      stringLitP
        <|> try floatLitP
        <|> intLitP
        <|> boolLitP

expression :: Parsec String u Expression
expression = literalP <?> "Syntax parsing error"

parseExpression :: Parsec String u Expression
parseExpression = withWhitespace expression
