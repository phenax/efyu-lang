module Efyu.Syntax.Type where

import Efyu.Syntax.Syntax
import Efyu.Syntax.Utils
import Efyu.Types.Types
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

tNameP _sp = do
  name <- identifier
  -- TODO: Product types
  pure $ case name of
    "Int" -> TInt
    "Float" -> TFloat
    "String" -> TString
    "Bool" -> TBool
    n -> TVar n

tLambdaP sp = do
  tys <- (sp >> tNameP sp <* sc) `sepBy1` L.symbol sp "->"
  pure $ mergety tys
  where
    mergety [] = undefined -- NOTE: Can't happen since sepBy1 guarentees nonempty
    mergety [ty] = ty
    mergety (ty : tys) = TLambda ty $ mergety tys

typeP :: MParser () -> MParser Type
typeP sp = try (tLambdaP sp) <?> "<type>"

typeAnnotationP :: MParser Expression
typeAnnotationP = withLineFold $ \sp -> do
  name <- identifier <* sp
  L.symbol sp ":"
  TypeAnnotation name <$> (typeP sp <* sc)
