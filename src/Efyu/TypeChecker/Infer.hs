module Efyu.TypeChecker.Infer where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Data.List (foldl', sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Syntax.Block
import Efyu.TypeChecker.Env
import Efyu.TypeChecker.FreeTypeVars
import Efyu.TypeChecker.Utils
import Efyu.Types

type TI = WithEnv (ExceptT String IO)

unificationErrorMessage :: Type -> Type -> String
unificationErrorMessage t1 t2 =
  "unable to unify types: " ++ show t1 ++ " and " ++ show t2

unboundVarErrorMessage :: IdentifierName 'VarName -> String
unboundVarErrorMessage (IdentifierName name) = "reference to unbound variable: " ++ name

occursCheckErrorMessage :: IdentifierName 'PolyTypeName -> String
occursCheckErrorMessage (IdentifierName name) = "occur check failed: Type var " ++ name ++ " already exists"

runTI :: TI a -> IO (Either String a)
runTI = runExceptT . runWithEnv

-- | Unify two types and return substitutions
unify :: Type -> Type -> TI TypeSubst
unify t t' = case (t, t') of
  (ty, TVar name) -> bindTypeVar name ty
  (TVar name, ty) -> bindTypeVar name ty
  (ty, ty') | ty == ty' -> pure Map.empty
  (TUnknown, _) -> pure Map.empty
  (_, TUnknown) -> pure Map.empty
  (TList a, TList b) -> unify a b
  (TTuple as, TTuple bs)
    | length as == length bs ->
      foldl' composeSubst Map.empty <$> zipWithM unify as bs
  (TLambda p r, TLambda p' r') -> do
    st <- unify p p'
    st' <- unify (apply st r) (apply st r')
    pure $ composeSubst st st'
  (ty, ty') ->
    lift . throwE $ unificationErrorMessage ty ty'
  where
    bindTypeVar :: IdentifierName 'PolyTypeName -> Type -> TI TypeSubst
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
inferLiteralType :: Literal -> TI Type
inferLiteralType = \case
  LiteralInt _ -> pure TInt
  LiteralString _ -> pure TString
  LiteralBool _ -> pure TBool
  LiteralFloat _ -> pure TFloat
  LiteralList exprs -> TList <$> foldM unifyE TUnknown exprs
    where
      unifyE :: Type -> Expression -> TI Type
      unifyE t1 expr =
        inferExpressionType expr >>= (\t2 -> higherSp t1 t2 <$ unify t1 t2)
  LiteralTuple [] -> pure TUnknown -- TODO: Invalid case (maybe unit)
  LiteralTuple exprs -> TTuple <$> foldM unifyE [] exprs
    where
      unifyE :: [Type] -> Expression -> TI [Type]
      unifyE ts expr = (\t -> ts ++ [t]) <$> inferExpressionType expr

inferExpressionType :: Expression -> TI Type
inferExpressionType expr = do
  (subst, ty) <- inferExpressionType' expr
  pure $ apply subst ty

inferExpressionType' :: Expression -> TI (TypeSubst, Type)
inferExpressionType' = \case
  Literal lit -> (Map.empty,) <$> inferLiteralType lit
  Lambda param body -> do
    typeVar <- newTypeVar "a"
    let newVal = Map.singleton param (TypeScheme [] typeVar)
    (bodySubst, bodyType) <- withValues newVal $ inferExpressionType' body
    pure (bodySubst, TLambda (apply bodySubst typeVar) bodyType)
  Apply lambda param -> do
    typeVar <- newTypeVar "a"
    (sl, tl) <- inferExpressionType' lambda
    modifyEnv $ apply sl -- NOTE: not sure if this should be scoped to the parameter
    (sp, tp) <- inferExpressionType' param
    sres <- unify (apply sp tl) (TLambda tp typeVar)
    pure (sres `composeSubst` sp `composeSubst` sl, apply sres typeVar)
  Var name -> do
    resMaybe <- lookupValue name
    case resMaybe of
      Nothing -> lift . throwE $ unboundVarErrorMessage name
      Just scheme -> (Map.empty,) <$> instantiate scheme
  Let bindings body -> do
    stDefs <- resolveDeclarationList bindings
    env <- getEnv
    (stBody, tyBody) <- withValues (envValues $ apply stDefs env) $ inferExpressionType' body
    pure (stDefs `composeSubst` stBody, tyBody)
  IfElse cond ifE elseE -> do
    (_, condT) <- inferExpressionType' cond
    unify condT TBool -- Check if condition is boolean
    (ifSt, ifT) <- inferExpressionType' ifE
    (elseSt, elseT) <- inferExpressionType' elseE
    subst <- unify ifT elseT
    let subst' = ifSt `Map.union` elseSt `Map.union` subst
    pure (Map.empty, apply subst' $ higherSp ifT elseT)

resolveDeclaration :: TypeSubst -> Definition -> TI TypeSubst
resolveDeclaration st = \case
  (DefSignature name ty) -> do
    let tsch = TypeScheme (Set.toList . freeTypeVars $ ty) ty
    modifyEnv $ updateValues (Map.insert name tsch)
    pure st
  (DefValue name expr) -> do
    -- Create a temporary scheme (needed for recursive definitions)
    tyMaybe <- lookupValue name
    typeDefn <- case tyMaybe of
      Just ty' -> instantiate ty'
      Nothing -> pure TUnknown

    env <- getEnv
    tmpTypeScheme <- generalize (apply st env) <$> newTypeVar "t"

    -- Infer type using tmpEnv
    (stBinding, properTypeScheme) <- withValues (Map.singleton name tmpTypeScheme) $ do
      (stBinding, tyBinding) <- inferExpressionType' expr
      -- Check if previous definition (signature) matches inferred type
      unify typeDefn tyBinding

      curEnv <- getEnv
      let properTypeScheme = generalize curEnv (higherSp typeDefn tyBinding)
      pure (stBinding, properTypeScheme)

    modifyEnv $ updateValues (Map.insert name properTypeScheme)
    pure $ st `Map.union` stBinding

-- | Resolve a set of bindings to a set of type substitutions and
resolveDeclarationList :: [Definition] -> TI TypeSubst
resolveDeclarationList = foldM resolveDeclaration Map.empty . sortBy cmpDefinition
  where
    cmpDefinition (DefSignature _ _) _ = LT
    cmpDefinition _ (DefSignature _ _) = GT
    cmpDefinition _ _ = EQ

-- | Check if expression matches given type
checkExpressionType :: Expression -> Type -> TI (Type, Expression)
checkExpressionType expr TUnknown = (,expr) <$> inferExpressionType expr
checkExpressionType expr ty = do
  ty' <- inferExpressionType expr
  unify ty ty'
  pure (higherSp ty ty', expr)

-- | Verify type signature of block
checkBlockType :: Block -> TI ()
checkBlockType (Module _ blocks) = foldM accBlockEnv () blocks
  where
    accBlockEnv _ b = checkBlockType b
checkBlockType (Def def) = do
  st <- resolveDeclaration Map.empty def
  modifyEnv $ apply st

-- | Type check module (module block)
checkModule :: Block -> TI ()
checkModule = checkBlockType

---
---
