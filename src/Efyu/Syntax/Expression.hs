module Efyu.Syntax.Expression where

import Efyu.Syntax.Syntax
import Efyu.Syntax.Utils
import Text.Megaparsec
import Text.Megaparsec.Char

literalP :: MParser Expression
literalP = Literal <$> p
  where
    p = try floatP <|> intP <|> stringP <|> boolP
    floatP = LiteralFloat <$> float
    intP = LiteralInt <$> integer
    stringP = LiteralString <$> insideQuotes
    boolP = LiteralBool . (== "True") <$> (symbol "True" <|> symbol "False")

identifier :: MParser String
identifier = lexeme $ do
  f <- letterChar
  rest <- many (alphaNumChar <|> oneOf "_'?")
  pure $ f : rest

varP :: MParser Expression
varP = Var <$> identifier

definitionP :: MParser (String, Expression)
definitionP = lexeme $ do
  name <- identifier
  char '='
  value <- expressionP
  scnl
  char ';'
  scnl
  pure (name, value)

letBindingP :: MParser Expression
letBindingP = do
  string "let"
  scnl
  vars <- definitionP `someTill` symbol "in"
  scnl
  Let vars <$> expressionP

expressionP :: MParser Expression
expressionP = scnl >> p
  where
    p = letBindingP <|> literalP <|> varP <?> "Syntax parsing error"
