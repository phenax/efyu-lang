module Efyu.Types.Infer where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Syntax.Syntax
import Efyu.Types.Types

data TIEnv = TIEnv {}

type TI = StateT Int (ExceptT String IO)

unificationErrorMessage t1 t2 =
  "unable to unify types: " ++ show t1 ++ " and " ++ show t2

runTI :: TI a -> IO (Either String a)
runTI t = do
  res <- run t
  pure $ fst <$> res
  where
    run = runExceptT . flip runStateT 0

-- | Create a new type variable
newTypeVar :: Identifier -> TI Type
newTypeVar prefix = do
  s <- get
  put $ s + 1
  pure . TVar $ prefix ++ show s

-- | Unify two types and return substitutions
unify :: Type -> Type -> TI TypeSubst
unify t t' = case (t, t') of
  (ty, TVar name) -> bindTypeVar name ty
  (TVar name, ty) -> bindTypeVar name ty
  (ty, ty') | ty == ty' -> pure Map.empty
  (TUnknown, _) -> pure Map.empty
  (_, TUnknown) -> pure Map.empty
  (TLambda p r, TLambda p' r') -> do
    st <- unify p p'
    st' <- unify (apply st r) (apply st r')
    pure $ composeSubst st st'
  (ty, ty') ->
    lift . throwE $ unificationErrorMessage ty ty'
  where
    -- bind a type variable to a type
    -- if it's the same type variable, no substitutions
    -- if a var with the same name is found inside ty, throw error
    -- else substitute name with ty
    bindTypeVar :: Identifier -> Type -> TI TypeSubst
    bindTypeVar name = \case
      TVar n | n == name -> pure Map.empty
      ty | Set.member name (freeTypeVars ty) -> lift $ throwE "foobaroty"
      ty -> pure $ Map.singleton name ty

-- | Replace all bound types with fresh polymorphic type vars
instantiate :: TypeScheme -> TI Type
instantiate (TypeScheme vars t) = do
  vars' <- mapM (\_ -> newTypeVar "a") vars
  let s = Map.fromList (zip vars vars')
  pure $ apply s t

-- | Generalize type over all free vars in type but not in type env
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = flip TypeScheme t . Set.toList $ freeTypes
  where
    freeTypes = Set.difference (freeTypeVars t) (freeTypeVars env)

-- | Infer types of literals
inferLiteralType :: Literal -> Type
inferLiteralType = \case
  LiteralInt _ -> TInt
  LiteralString _ -> TString
  LiteralBool _ -> TBool
  LiteralFloat _ -> TFloat

inferType' :: TypeEnv -> Expression -> TI (TypeSubst, Type)
inferType' env = \case
  Literal lit -> pure (Map.empty, inferLiteralType lit)
  Lambda param body -> do
    tv <- newTypeVar "a"
    let env' = Map.insert param (TypeScheme [] tv) env
    (bodySubst, bodyType) <- inferType' env' body
    pure (bodySubst, TLambda (apply bodySubst tv) bodyType)
  Apply lambda param -> do
    typeVar <- newTypeVar "a"
    (sl, tl) <- inferType' env lambda
    (sp, tp) <- inferType' (apply sl env) param
    sres <- unify (apply sp tl) (TLambda tp typeVar)
    pure (sres `composeSubst` sp `composeSubst` sl, apply sres typeVar)
  Var name ->
    case Map.lookup name env of
      Nothing -> lift . throwE $ "Unbound variable " ++ name
      Just scheme -> do
        ty <- instantiate scheme
        pure (Map.empty, ty)
  Let bindings body -> do
    (stBinding, env') <- resolveBindings env bindings
    (stBody, tyBody) <- inferType' (apply stBinding env') body
    pure (stBinding `composeSubst` stBody, tyBody)
  IfElse _cond ifE _elseE -> inferType' env ifE -- TODO: Fix laterz
  TypeAnnotation _n ty -> pure (Map.empty, ty)

-- | Resolve a set of bindings to a set of type substitutions and
resolveBindings :: TypeEnv -> [(Identifier, Expression)] -> TI (TypeSubst, TypeEnv)
resolveBindings env = foldM getSubstEnv (Map.empty, env)
  where
    getSubstEnv (st, env') (name, expr) = do
      (stBinding, tyBinding) <- inferType' env' expr
      let ty' = generalize (apply stBinding env') tyBinding
      let tyName = case expr of
            TypeAnnotation n _ -> n
            _ -> name
      pure (Map.union st stBinding, Map.insert tyName ty' env')

inferType :: TypeEnv -> Expression -> TI Type
inferType env expr = do
  (subst, ty) <- inferType' env expr
  pure $ apply subst ty

---
---
