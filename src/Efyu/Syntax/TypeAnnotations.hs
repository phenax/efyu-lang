module Efyu.Syntax.TypeAnnotations where

import Data.Foldable (Foldable (foldl'))
import Efyu.Syntax.Utils
import Efyu.Types
import Efyu.Utils (debugM)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

tListP :: MParser () -> MParser Type
tListP sp = TList <$> p
  where
    p = L.symbol sc "[" >> typeP sp <* sc <* L.symbol sc "]"

tTupleP :: MParser () -> MParser Type
tTupleP sp = TTuple <$> p
  where
    p = L.symbol sc "(" >> (typeP sp `sepBy1` L.symbol sp ",") <* sc <* L.symbol sc ")"

tNameP = do
  name <- typeIdentifier
  pure $ case name of
    IdentifierName "Int" -> TInt
    IdentifierName "Float" -> TFloat
    IdentifierName "String" -> TString
    IdentifierName "Bool" -> TBool
    n -> TName n

tVarP sp = TVar <$> polyTypeIdentifier <* sp

tApplyP :: MParser () -> MParser Type
tApplyP sp = do
  name <- tNameP
  params <- try $ argListParser []
  pure $ foldl' TApply name params
  where
    argListParser ls = do
      let argP = sp >> (tNameP <|> try (tAtomP sp) <?> "<fuckkk>")
      optn <- optional . try $ argP
      sc
      case optn of
        Nothing -> pure ls
        Just p -> argListParser $ ls ++ [p]

tAtomP :: MParser () -> MParser Type
tAtomP sp =
  optParens (tListP sp)
    <|> optParens (tApplyP sp)
    <|> optParens tNameP
    <|> optParens (tVarP sp)
    <|> parens (tLambdaP sp)
    <|> tTupleP sp
  where
    parens = try . withParens
    optParens = try . withOptionalParens

tLambdaP :: MParser () -> MParser Type
tLambdaP sp = do
  tys <- (sp >> tAtomP sp <* sc) `sepBy1` L.symbol sp "->"
  pure $ mergety tys
  where
    mergety [] = undefined -- NOTE: Can't happen since sepBy1 guarentees nonempty
    mergety [ty] = ty
    mergety (ty : tys) = TLambda ty $ mergety tys

typeP :: MParser () -> MParser Type
typeP sp = try (tLambdaP sp) <|> try (tApplyP sp) <?> "<type>"

typeAnnotationP :: MParser Definition
typeAnnotationP = withLineFold $ \sp -> do
  name <- varIdentifier <* sp
  L.symbol sp ":"
  DefSignature name <$> typeP sp <* scnl

--
