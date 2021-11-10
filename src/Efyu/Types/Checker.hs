module Efyu.Types.Checker where

import Efyu.Syntax.Syntax
import Efyu.Types.Infer
import Efyu.Types.Types

checkExpressionType :: TypeEnv -> Expression -> Type -> TI (Type, Expression)
checkExpressionType env expr@(IfElse cond _ _) ty = do
  checkExpressionType env cond TBool
  ty' <- inferExpressionType env expr
  unify ty ty'
  pure (ty, expr)
checkExpressionType env expr TUnknown = (,expr) <$> inferExpressionType env expr
checkExpressionType env expr ty = do
  ty' <- inferExpressionType env expr
  unify ty ty'
  pure (ty, expr)

checkExpressionType' :: TypeEnv -> Expression -> Type -> TI Type
checkExpressionType' env expr ty = fst <$> checkExpressionType env expr ty

---
