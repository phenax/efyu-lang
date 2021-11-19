module Efyu.Errors where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Void (Void)
import Efyu.Types
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

data CompilerError
  = TypeUnificationError Type Type
  | UnboundVariableError (IdentifierName 'VarName)
  | UnboundTypeError (IdentifierName 'TypeName)
  | UnboundPolyTypeError (IdentifierName 'PolyTypeName)
  | UnboundConstructorError (IdentifierName 'ConstructorName)
  | OccursError (IdentifierName 'PolyTypeName)
  | KindMismatchError Type Type
  | IllegalKindError Type
  | ParseError (ParseErrorBundle String Void)
  deriving (Eq)

instance Show CompilerError where
  show = \case
    TypeUnificationError t1 t2 ->
      "unable to unify types: " ++ show t1 ++ " and " ++ show t2
    UnboundVariableError (IdentifierName name) ->
      "reference to unbound variable: " ++ name
    UnboundTypeError (IdentifierName name) ->
      "reference to undefined type: " ++ name
    UnboundPolyTypeError (IdentifierName name) ->
      "reference to undefined type variable: " ++ name
    OccursError (IdentifierName name) ->
      "occur check failed: Type var " ++ name ++ " already exists"
    KindMismatchError ty typaram ->
      "kind signature doesn't match. unable to apply: " ++ show ty ++ " (" ++ show typaram ++ ")"
    IllegalKindError ty ->
      "expected type but got kind: " ++ show ty
    UnboundConstructorError (IdentifierName name) ->
      "reference to undefined constructor: " ++ name
    ParseError e -> errorBundlePretty e

type WithCompilerError = ExceptT CompilerError

runWithError :: (MonadIO m) => WithCompilerError m a -> m (Either CompilerError a)
runWithError = runExceptT

throwErr :: (MonadIO m) => CompilerError -> WithCompilerError m a
throwErr = throwE
