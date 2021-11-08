module Efyu.Types.Checker where

import Efyu.Syntax.Syntax
import Efyu.Types.Infer
import Efyu.Types.Types

checkType' :: TypeEnv -> Expression -> Type -> TI (Type, Expression)
checkType' env expr TUnknown = (,expr) <$> inferType env expr
checkType' env expr ty = do
  ty' <- inferType env expr
  unify ty ty'
  pure (ty, expr)

checkType :: TypeEnv -> Expression -> Type -> TI Expression
checkType env expr ty = snd <$> checkType' env expr ty

---
