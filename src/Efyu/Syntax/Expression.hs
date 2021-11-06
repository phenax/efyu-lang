module Efyu.Syntax.Expression where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Efyu.Syntax.Syntax
import Efyu.Syntax.Utils
import Efyu.Utils (debugM)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

literalP :: MParser Expression
literalP = Literal <$> p
  where
    p = try floatP <|> intP <|> stringP <|> try boolP
    floatP = LiteralFloat <$> float
    intP = LiteralInt <$> integer
    stringP = LiteralString <$> insideQuotes
    boolP = LiteralBool . (== "True") <$> (symbol "True" <|> symbol "False")

reservedKeywords :: [String]
reservedKeywords = ["let", "in", "if", "then", "else"]

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
  name <- identifier
  sp >> char '='
  value <- sp >> bareExpressionP <* sc
  optional (char ';')
  pure (name, value)

letBindingP :: MParser Expression
letBindingP = withLineFold $ \sp -> do
  L.symbol sp "let"
  vars <- (definitionP <* scnl) `someTill` L.symbol sp "in"
  Let vars <$> bareExpressionP

lambdaP :: MParser Expression
lambdaP = withLineFold $ \sp -> do
  char '\\'
  var <- lexeme parameter
  sp
  symbol "->"
  Lambda var <$> (sp >> bareExpressionP)

applyP :: MParser Expression
applyP = withLineFold $ \sp -> do
  fn <- try varP <|> withParens expressionP
  debugM fn
  params <- argParser sp []
  if null params
    then pure fn
    else pure $ foldl' Apply fn params
  where
    argParser sp ls = do
      optn <- optional . try $ sp >> (literalP <|> varP <|> withParens expressionP)
      sc
      case optn of
        Nothing -> pure ls
        Just p -> argParser sp $ ls ++ [p]

bareExpressionP :: MParser Expression
bareExpressionP =
  (try . parens) literalP
    <|> (try . parens) letBindingP
    <|> try applyP
    <|> (try . parens) lambdaP
    <|> (try . parens) varP
    <?> "<expr>"
  where
    parens = withOptionalParens

expressionP :: MParser Expression
expressionP = scnl >> bareExpressionP <* scnl

--
--
--
--
