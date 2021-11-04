module Efyu.Syntax.Expression where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Efyu.Syntax.Syntax
import Efyu.Syntax.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

literalP :: MParser Expression
literalP = Literal <$> lexeme p
  where
    p = try floatP <|> intP <|> stringP <|> boolP
    floatP = LiteralFloat <$> float
    intP = LiteralInt <$> integer
    stringP = LiteralString <$> insideQuotes
    boolP = LiteralBool . (== "True") <$> (symbol "True" <|> symbol "False")

reservedKeywords :: [String]
reservedKeywords = ["let", "in"]

identifier :: MParser String
identifier = do
  f <- letterChar
  rest <- many (alphaNumChar <|> oneOf "_'?")
  let name = f : rest
  if name `elem` reservedKeywords
    then unexpected $ Label (f :| rest)
    else pure name

parameter :: MParser String
parameter = identifier

varP :: MParser Expression
varP = Var <$> lexeme identifier

definitionP :: MParser (String, Expression)
definitionP = withLineFold $ \sp -> do
  name <- lexeme identifier
  char '='
  sp
  value <- expressionP
  optional (char ';')
  scnl
  pure (name, value)

letBindingP :: MParser Expression
letBindingP = withLineFold $ \sp -> do
  L.symbol sp "let"
  vars <- definitionP `someTill` L.symbol sp "in"
  Let vars <$> expressionP

lambdaP :: MParser Expression
lambdaP = withLineFold $ \sp -> do
  char '\\'
  var <- lexeme parameter
  sp
  string "->"
  Lambda var <$> L.lexeme sp expressionP

applyP :: MParser Expression
applyP = withLineFold $ \sp -> do
  char '@'
  fn <- L.lexeme sp expressionP
  sp
  params <- some $ L.lexeme sp expressionP
  pure $ foldl' Apply fn params

expressionP :: MParser Expression
expressionP = scnl >> withOptionalParens p <* scnl
  where
    p = letBindingP <|> lambdaP <|> literalP <|> try applyP <|> varP <?> "Syntax parsing error"

--
--
--
--
