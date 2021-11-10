module Efyu.Types.Checker where

import Control.Monad (foldM)
import Efyu.Syntax.Block
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

-- TODO: Use type annotations
checkBlockType :: TypeEnv -> Block -> Type -> TI (TypeEnv, Type)
checkBlockType env (Def _ expr) ty = (env,) <$> checkExpressionType' env expr ty
checkBlockType env (Module _ blocks) _ = foldM accBlockEnv (env, TUnknown) blocks
  where
    accBlockEnv (env', _) b = checkBlockType env' b TUnknown

---
