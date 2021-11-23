module Efyu.Syntax.Expression where

import Data.Foldable (Foldable (foldr'))
import Data.List (foldl')
import Efyu.Syntax.TypeAnnotations ()
import Efyu.Syntax.Utils
import Efyu.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

instance (Parsable a) => Parsable (Literal a) where
  parser _ = try tupleP <|> tryParens (try floatP <|> intP <|> stringP <|> try boolP <|> listP)
    where
      comma = L.symbol scnl ","
      tryParens = try . withOptionalParens
      floatP = LiteralFloat <$> float
      intP = LiteralInt <$> integer
      stringP = LiteralString <$> insideQuotes
      boolP = LiteralBool . (== "True") <$> (symbol "True" <|> symbol "False")
      tupleP =
        LiteralTuple
          <$> withLineFold
            ( \sp -> do
                x <- L.symbol sp "(" >> parser sp <* comma
                xs <- (sp >> parser sp) `sepEndBy1` comma
                L.symbol scnl ")"
                pure $ x : xs
            )
      listP =
        LiteralList
          <$> withLineFold
            ( \sp ->
                L.symbol sp "["
                  >> ((sp >> parser sp) `sepEndBy` comma)
                  <* L.symbol scnl "]"
            )

instance Parsable Expression where
  parser _ = scnl >> p <* scnl
    where
      p =
        try literalExprP
          <|> (try . parens) letBindingP
          <|> (try . parens) ifThenElseP
          <|> try applyP
          <|> (try . parens) lambdaP
          <|> (try . parens) varP
          <|> (try . parens) caseOfP
          <?> "<expr>"
      parens = withOptionalParens

instance Parsable (Arg Expression) where
  parser sp = Arg <$> (literalExprP <|> varP <|> constructorP <|> withParens (parser sp))

instance Parsable Pattern where
  parser sp = try p
    where
      p =
        (PatVar <$> varIdentifier)
          <|> (PatWildcard <$ symbol "_")
          <|> (PatLiteral <$> parser sp)
          <|> patternCtorApplyP sp

instance Parsable (Arg Pattern) where
  parser sp = Arg <$> try p
    where
      p =
        (PatVar <$> varIdentifier)
          <|> (PatWildcard <$ symbol "_")
          <|> (PatLiteral <$> parser sp)
          <|> (flip PatCtor [] <$> constructorIdentifier)
          <|> withParens (patternCtorApplyP sp)

instance Parsable Definition where
  parser _ = try defP <|> typeAnnotationP
    where
      typeAnnotationP = withLineFold $ \sp -> do
        name <- varIdentifier <* sp
        L.symbol sp ":"
        DefSignature name <$> parser sp <* scnl
      defP = withLineFold $ \sp -> do
        name <- varIdentifier <* sp
        params <- (parameter <* sp) `manyTill` char '='
        body <- sp >> parser sp
        optional (char ';')
        pure $ DefValue name (foldr' Lambda body params)

patternCtorApplyP sp = do
  name <- constructorIdentifier
  args <- argListP (parser sp :: MParser (Arg Pattern)) sp
  pure . PatCtor name . map argToExpr $ args

parameter :: MParser (IdentifierName 'VarName)
parameter = varIdentifier

literalExprP :: MParser Expression
literalExprP = Literal <$> parser sc

varP :: MParser Expression
varP = Var <$> lexeme varIdentifier

constructorP :: MParser Expression
constructorP = Ctor <$> lexeme constructorIdentifier

letBindingP :: MParser Expression
letBindingP = do
  pos <- indentLevel
  symbol "let"
  vars <- (indentGt pos >> parser scnl <* scnl) `someTill` (indentGtEq pos >> symbol "in")
  Let vars <$> (indentGt pos >> expressionP)

lambdaP :: MParser Expression
lambdaP = withLineFold $ \sp -> do
  var <- symbol "\\" >> parameter <* sp <* L.symbol sp "->"
  Lambda var <$> (sp >> expressionP)

applyP :: MParser Expression
applyP = withLineFold $ \sp -> do
  fn <- try varP <|> try constructorP <|> withParens expressionP
  params <- argListP (parser sp :: MParser (Arg Expression)) sp
  pure . foldl' Apply fn . map argToExpr $ params

ifThenElseP :: MParser Expression
ifThenElseP = do
  pos <- indentLevel
  symbol "if"
  condE <- indentGt pos >> expressionP
  symbol "then"
  ifE <- indentGt pos >> expressionP
  symbol "else"
  elseE <- indentGt pos >> expressionP
  pure $ IfElse condE ifE elseE

caseOfP :: MParser Expression
caseOfP = withLineFold $ \sp -> do
  expr <- L.symbol sp "case" >> expressionP <* L.symbol sp "of"
  items <- some $ sp >> caseItemP sp
  pure $ CaseOf expr items
  where
    caseItemP sp = do
      pattern <- parser sp <* sp <* L.symbol sp "->"
      body <- parser sp
      -- TODO: Parse guard
      pure $ CaseItem pattern Nothing body

expressionP :: MParser Expression
expressionP = parser scnl

--
--
--
--
