module Efyu.Types.Checker where

import Efyu.Syntax.Syntax
import Efyu.Types.Infer
import Efyu.Types.Types

checkType :: TypeEnv -> Expression -> Type -> TI (Type, Expression)
checkType env expr@(IfElse cond ifE elseE) ty = do
  checkType env cond TBool
  (tyIf, _) <- checkType env ifE ty
  checkType env elseE tyIf
  pure (tyIf, expr)
checkType env expr TUnknown = (,expr) <$> inferType env expr
checkType env expr ty = do
  ty' <- inferType env expr
  unify ty ty'
  pure (ty, expr)

checkType' :: TypeEnv -> Expression -> Type -> TI Type
checkType' env expr ty = fst <$> checkType env expr ty

---
