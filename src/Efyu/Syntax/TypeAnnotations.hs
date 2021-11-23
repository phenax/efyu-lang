module Efyu.Syntax.TypeAnnotations where

import Data.Foldable (Foldable (foldl'))
import Efyu.Syntax.Utils
import Efyu.Types
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

instance Parsable Type where
  parser sp = try (tLambdaP sp) <|> try (tApplyP sp) <?> "<type>"

instance Parsable (Arg Type) where
  parser sp = Arg <$> (tNameP <|> try (tAtomP sp) <?> "<fuckkk>")

tListP :: MParser () -> MParser Type
tListP sp = TList <$> p
  where
    p = L.symbol sc "[" >> parser sp <* sc <* L.symbol sc "]"

tTupleP :: MParser () -> MParser Type
tTupleP sp = TTuple <$> p
  where
    p = L.symbol sc "(" >> (parser sp `sepBy1` L.symbol sp ",") <* sc <* L.symbol sc ")"

tNameP :: MParser Type
tNameP = do
  name <- typeIdentifier
  pure $ case name of
    IdentifierName "Int" -> TInt
    IdentifierName "Float" -> TFloat
    IdentifierName "String" -> TString
    IdentifierName "Bool" -> TBool
    n -> TName n

tVarP :: MParser Type
tVarP = TVar <$> polyTypeIdentifier

tConstructorP :: MParser () -> MParser Constructor
tConstructorP sp = do
  name <- constructorIdentifier
  params <- map argToExpr <$> argListP (parser sp :: MParser (Arg Type)) sp
  pure $ Constructor TUnknown name params

tSumTypeP :: MParser () -> MParser Type
tSumTypeP sp = do
  optional $ L.symbol sp "|"
  TCtors <$> ((tConstructorP sp <* scnl) `sepBy1` L.symbol sp "|")

tApplyP :: MParser () -> MParser Type
tApplyP sp = do
  name <- tNameP
  params <- map argToExpr <$> argListP (parser sp :: MParser (Arg Type)) sp
  pure $ foldl' TApply name params

tAtomP :: MParser () -> MParser Type
tAtomP sp =
  optParens (tListP sp)
    <|> optParens tVarP
    <|> optParens (tApplyP sp)
    <|> optParens tNameP
    <|> parens (tLambdaP sp)
    <|> tTupleP sp
  where
    parens = try . withParens
    optParens = try . withOptionalParens

tLambdaP :: MParser () -> MParser Type
tLambdaP sp = do
  tys <- (sp >> tAtomP sp <* sc) `sepBy1` L.symbol sp "->"
  pure $ foldr1 TLambda tys

--
