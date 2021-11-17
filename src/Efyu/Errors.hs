module Efyu.Errors where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Efyu.Types

data CompilerError
  = TypeUnificationError Type Type
  | UnboundVariableError (IdentifierName 'VarName)
  | UnboundTypeError (IdentifierName 'TypeName)
  | OccursError (IdentifierName 'PolyTypeName)
  | KindMismatchError Type Type
  | IllegalKindError Type
  deriving (Eq)

instance Show CompilerError where
  show = \case
    TypeUnificationError t1 t2 ->
      "unable to unify types: " ++ show t1 ++ " and " ++ show t2
    UnboundVariableError (IdentifierName name) ->
      "reference to unbound variable: " ++ name
    UnboundTypeError (IdentifierName name) ->
      "reference to undefined type: " ++ name
    OccursError (IdentifierName name) ->
      "occur check failed: Type var " ++ name ++ " already exists"
    KindMismatchError ty typaram ->
      "kind signature doesn't match. unable to apply: " ++ show ty ++ " (" ++ show typaram ++ ")"
    IllegalKindError ty -> "expected type but got kind: " ++ show ty

type WithCompilerError = ExceptT CompilerError

runWithError :: (MonadIO m) => WithCompilerError m a -> m (Either CompilerError a)
runWithError = runExceptT

throwErr :: (MonadIO m) => CompilerError -> WithCompilerError m a
throwErr = throwE
