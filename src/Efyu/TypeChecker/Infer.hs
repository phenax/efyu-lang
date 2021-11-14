module Efyu.TypeChecker.Infer where

import Control.Monad (foldM, void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Syntax.Block
import Efyu.TypeChecker.Utils
import Efyu.Types

type TI = StateT Int (ExceptT String IO)

unificationErrorMessage :: Type -> Type -> String
unificationErrorMessage t1 t2 =
  "unable to unify types: " ++ show t1 ++ " and " ++ show t2

unboundVarErrorMessage :: Identifier -> String
unboundVarErrorMessage name = "reference to unbound variable: " ++ name

occursCheckErrorMessage :: Identifier -> String
occursCheckErrorMessage name = "occur check failed: Type var " ++ name ++ " already exists"

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
  (TList a, TList b) -> unify a b
  -- TODO: TTuple
  (TLambda p r, TLambda p' r') -> do
    st <- unify p p'
    st' <- unify (apply st r) (apply st r')
    pure $ composeSubst st st'
  (ty, ty') ->
    lift . throwE $ unificationErrorMessage ty ty'
  where
    bindTypeVar :: Identifier -> Type -> TI TypeSubst
    bindTypeVar name = \case
      TVar n | n == name -> pure Map.empty
      ty
        | Set.member name (freeTypeVars ty) ->
          lift . throwE $ occursCheckErrorMessage name
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
inferLiteralType :: TypeEnv -> Literal -> TI Type
inferLiteralType env = \case
  LiteralInt _ -> pure TInt
  LiteralString _ -> pure TString
  LiteralBool _ -> pure TBool
  LiteralFloat _ -> pure TFloat
  LiteralList exprs -> TList <$> foldM unifyE TUnknown exprs
    where
      unifyE :: Type -> Expression -> TI Type
      unifyE t1 expr =
        inferExpressionType env expr >>= (\t2 -> higherSp t1 t2 <$ unify t1 t2)
  LiteralTuple [] -> pure TUnknown -- TODO: Invalid case (maybe unit)
  LiteralTuple exprs -> TTuple <$> foldM unifyE [] exprs
    where
      unifyE :: [Type] -> Expression -> TI [Type]
      unifyE ts expr = (\t -> ts ++ [t]) <$> inferExpressionType env expr

inferExpressionType :: TypeEnv -> Expression -> TI Type
inferExpressionType env expr = do
  (subst, ty) <- inferExpressionType' env expr
  pure $ apply subst ty

inferExpressionType' :: TypeEnv -> Expression -> TI (TypeSubst, Type)
inferExpressionType' env = \case
  Literal lit -> (Map.empty,) <$> inferLiteralType env lit
  Lambda param body -> do
    tv <- newTypeVar "a"
    let env' = Map.insert param (TypeScheme [] tv) env
    (bodySubst, bodyType) <- inferExpressionType' env' body
    pure (bodySubst, TLambda (apply bodySubst tv) bodyType)
  Apply lambda param -> do
    typeVar <- newTypeVar "a"
    (sl, tl) <- inferExpressionType' env lambda
    (sp, tp) <- inferExpressionType' (apply sl env) param
    sres <- unify (apply sp tl) (TLambda tp typeVar)
    pure (sres `composeSubst` sp `composeSubst` sl, apply sres typeVar)
  Var name ->
    case Map.lookup name env of
      Nothing -> lift . throwE $ unboundVarErrorMessage name
      Just scheme -> do
        ty <- instantiate scheme
        pure (Map.empty, ty)
  Let bindings body -> do
    (stDefs, env') <- resolveDeclarationList env bindings
    (stBody, tyBody) <- inferExpressionType' (apply stDefs env') body
    pure (stDefs `composeSubst` stBody, tyBody)
  IfElse cond ifE elseE -> do
    (_, condT) <- inferExpressionType' env cond
    unify condT TBool -- Check if condition is boolean
    (ifSt, ifT) <- inferExpressionType' env ifE
    (elseSt, elseT) <- inferExpressionType' env elseE
    subst <- unify ifT elseT
    let subst' = ifSt `Map.union` elseSt `Map.union` subst
    pure (Map.empty, apply subst' $ higherSp ifT elseT)

resolveDeclaration :: TypeEnv -> TypeSubst -> Definition -> TI (TypeSubst, TypeEnv)
resolveDeclaration env st = \case
  (DefSignature name ty) -> do
    let tsch = TypeScheme (Set.toList . freeTypeVars $ ty) ty
    pure (st, Map.insert name tsch env)
  (DefValue name expr) -> do
    -- Create a temporary scheme (needed for recursive definitions)
    typeDefn <- case Map.lookup name env of
      Just ty' -> instantiate ty'
      Nothing -> pure TUnknown

    tmpTypeScheme <- generalize (apply st env) <$> newTypeVar "t"
    let tmpEnv = Map.insert name tmpTypeScheme env

    -- Infer type using tmpEnv
    (stBinding, tyBinding) <- inferExpressionType' tmpEnv expr
    unify typeDefn tyBinding -- Check if previous definition (signature) matches inferred type
    let properTypeScheme = generalize (apply st tmpEnv) (higherSp typeDefn tyBinding)
    let properEnv = Map.insert name properTypeScheme env

    pure (st `Map.union` stBinding, properEnv)

-- | Resolve a set of bindings to a set of type substitutions and
resolveDeclarationList :: TypeEnv -> [Definition] -> TI (TypeSubst, TypeEnv)
resolveDeclarationList env = foldM getSubstEnv (Map.empty, env) . sortBy cmpDefinition
  where
    getSubstEnv (st, env') def = resolveDeclaration env' st def
    cmpDefinition (DefSignature _ _) _ = LT
    cmpDefinition _ (DefSignature _ _) = GT
    cmpDefinition _ _ = EQ

-- | Check if expression matches given type
checkExpressionType :: TypeEnv -> Expression -> Type -> TI (Type, Expression)
checkExpressionType env expr TUnknown = (,expr) <$> inferExpressionType env expr
checkExpressionType env expr ty = do
  ty' <- inferExpressionType env expr
  unify ty ty'
  pure (higherSp ty ty', expr)

-- | Verify type signature of block
checkBlockType :: TypeEnv -> Block -> Type -> TI TypeEnv
checkBlockType env (Module _ blocks) _ = foldM accBlockEnv env blocks
  where
    accBlockEnv env' b = checkBlockType env' b TUnknown
checkBlockType env (Def def) _ = do
  (st, env') <- resolveDeclaration env Map.empty def
  pure $ apply st env'

-- | Type check module (module block)
checkModule :: Block -> TI ()
checkModule b = void $ checkBlockType Map.empty b TUnknown

---
---
