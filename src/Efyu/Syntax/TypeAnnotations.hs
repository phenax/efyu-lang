module Efyu.Syntax.TypeAnnotations where

import Efyu.Syntax.Utils
import Efyu.Types
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

tNameP _sp = do
  name <- identifier
  -- TODO: Product types
  pure $ case name of
    "Int" -> TInt
    "Float" -> TFloat
    "String" -> TString
    "Bool" -> TBool
    n -> TVar n

tLambdaP :: MParser () -> MParser Type
tLambdaP sp = do
  tys <- (sp >> tAtomP sp <* sc) `sepBy1` L.symbol sp "->"
  pure $ mergety tys
  where
    mergety [] = undefined -- NOTE: Can't happen since sepBy1 guarentees nonempty
    mergety [ty] = ty
    mergety (ty : tys) = TLambda ty $ mergety tys

tAtomP :: MParser () -> MParser Type
tAtomP sp =
  (try . withOptionalParens $ tNameP sp)
    <|> tListP sp
    <|> (try . parens $ tLambdaP sp)
    <|> tTupleP sp
  where
    parens = withParens

typeP :: MParser () -> MParser Type
typeP sp = try (tLambdaP sp) <?> "<type>"

typeAnnotationP :: MParser Definition
typeAnnotationP = withLineFold $ \sp -> do
  name <- identifier <* sp
  L.symbol sp ":"
  DefSignature name <$> (typeP sp <* sc)
