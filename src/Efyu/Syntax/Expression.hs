module Efyu.Syntax.Expression where

import Efyu.Syntax.Syntax
import Efyu.Syntax.Utils
import Text.Parsec

type EfyuParser u = Parsec String u

stringLitP :: EfyuParser u Literal
stringLitP = do
  char '"'
  LiteralString <$> anyChar `manyTill` char '"'

intLitP :: EfyuParser u Literal
intLitP =
  LiteralInt . read <$> do
    sign <- option "" $ string "-"
    d <- many1 digit
    return $ sign ++ d

floatLitP :: EfyuParser u Literal
floatLitP =
  LiteralFloat . read <$> do
    sign <- option "" $ string "-"
    n <- many1 digit
    char '.'
    dec <- many digit
    let decimal = if dec == "" then "0" else dec
    pure $ sign ++ n ++ "." ++ decimal

boolLitP :: EfyuParser u Literal
boolLitP = LiteralBool . toBool <$> (string "True" <|> string "False")
  where
    toBool = (== "True")

literalP :: EfyuParser u Expression
literalP = Literal <$> literal
  where
    literal =
      stringLitP
        <|> try floatLitP
        <|> intLitP
        <|> boolLitP

-- TODO: Allow _-'?
-- TODO: Prevent numeric as first character
-- TODO: Reserved keywords
identifier :: EfyuParser u Identifier
identifier = many1 alphaNum

varP :: EfyuParser u Expression
varP = withWhitespace (Var <$> identifier)

definitionP :: EfyuParser u (String, Expression)
definitionP = withWhitespace $ do
  name <- identifier
  whitespace
  char '='
  value <- expressionP
  char ';'
  pure (name, value)

letBindingP :: EfyuParser u Expression
letBindingP = withWhitespace $ do
  string "let"
  vars <- definitionP `manyTill` string "in"
  Let vars <$> expressionP

-- TODO: Use patterns instead of parsing identifier
lambdaP :: EfyuParser u Expression
lambdaP = withWhitespace $ do
  char '\\'
  var <- identifier
  whitespace
  string "->"
  Lambda var <$> expressionP

-- applyP :: EfyuParser u Expression
-- applyP = withWhitespace $ do
--   fn <- expressionP
--   Apply fn <$> expressionP

expressionP :: EfyuParser u Expression
expressionP = withWhitespace . withParens $ p
  where
    p = literalP <|> lambdaP <|> letBindingP <|> varP <?> "Syntax parsing error"

parseExpression :: EfyuParser u Expression
parseExpression = withWhitespace expressionP
