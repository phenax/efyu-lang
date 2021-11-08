module Efyu.Syntax.Expression where

import Data.Foldable (Foldable (foldr'))
import Data.Functor (void)
import Data.List (foldl')
import Efyu.Syntax.Syntax
import Efyu.Syntax.Type (typeAnnotationP)
import Efyu.Syntax.Utils
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

parameter :: MParser String
parameter = identifier

varP :: MParser Expression
varP = Var <$> lexeme identifier

definitionP :: MParser (Identifier, Expression)
definitionP = try defP <|> (("_",) <$> typeAnnotationP)
  where
    defP = withLineFold $ \sp -> do
      name <- identifier <* sp
      params <- (parameter <* sp) `manyTill` char '='
      body <- sp >> expressionP
      optional (char ';')
      pure (name, foldr' Lambda body params)

letBindingP :: MParser Expression
letBindingP = withLineFold $ \sp -> do
  L.symbol sp "let"
  vars <- (definitionP <* scnl) `someTill` L.symbol sp "in"
  Let vars <$> expressionP

lambdaP :: MParser Expression
lambdaP = withLineFold $ \sp -> do
  var <- symbol "\\" >> parameter <* sp <* L.symbol sp "->"
  Lambda var <$> (sp >> expressionP)

applyP :: MParser Expression
applyP = withLineFold $ \sp -> do
  fn <- try varP <|> withParens expressionP
  params <- argListParser sp []
  if null params
    then pure fn
    else pure $ foldl' Apply fn params
  where
    argListParser sp ls = do
      let argP = sp >> (literalP <|> varP <|> withParens expressionP)
      optn <- optional . try $ argP
      sc
      case optn of
        Nothing -> pure ls
        Just p -> argListParser sp $ ls ++ [p]

indentGtEq :: Pos -> MParser ()
indentGtEq pos = do
  void $ try (L.indentGuard scnl EQ pos) <|> L.indentGuard scnl GT pos
  scnl

indentGt :: Pos -> MParser ()
indentGt pos = do
  void $ L.indentGuard scnl GT pos
  scnl

ifThenElseP :: MParser Expression
ifThenElseP = do
  pos <- L.indentLevel
  symbol "if"
  condE <- indentGt pos >> expressionP
  symbol "then"
  ifE <- indentGt pos >> expressionP
  symbol "else"
  elseE <- indentGt pos >> expressionP
  pure $ IfElse condE ifE elseE

expressionP :: MParser Expression
expressionP = scnl >> p <* scnl
  where
    p =
      (try . parens) literalP
        <|> (try . parens) letBindingP
        <|> (try . parens) ifThenElseP
        <|> try applyP
        <|> (try . parens) lambdaP
        <|> (try . parens) varP
        <?> "<expr>"
    parens = withOptionalParens

--
--
--
--
