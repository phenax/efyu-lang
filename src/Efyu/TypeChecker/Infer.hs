module Efyu.TypeChecker.Infer where

import Control.Monad (foldM, zipWithM, (>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (Bifunctor (second))
import Data.List (foldl', sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Efyu.Errors
import Efyu.TypeChecker.Env
import Efyu.TypeChecker.FreeTypeVars
import Efyu.TypeChecker.Utils
import Efyu.Types
import Efyu.Utils (debugM)

type TI = WithEnv (WithCompilerError IO)

class TypeInference a where
  infer :: a -> TI (TypeSubst, Type)

runTI :: TI a -> IO (Either CompilerError a)
runTI = runWithError . runWithEnv

-- | Unify two types and return substitutions
unify :: Type -> Type -> TI TypeSubst
unify t t' = case (t, t') of
  (ty, TVar name) -> bindTypeVar name ty
  (TVar name, ty) -> bindTypeVar name ty
  (ty, ty') | ty == ty' -> pure Map.empty
  (TUnknown, _) -> pure Map.empty
  (_, TUnknown) -> pure Map.empty
  (TList a, TList b) -> unify a b
  (TName name, ty) -> bindLookupName lookupType UnboundTypeError name ty
  (ty, TName name) -> bindLookupName lookupType UnboundTypeError name ty
  (TTuple as, TTuple bs)
    | length as == length bs ->
      foldl' composeSubst Map.empty <$> zipWithM unify as bs
  (TLambda p r, TLambda p' r') -> do
    st <- unify p p'
    st' <- unify (apply st r) (apply st r')
    pure $ composeSubst st st'
  (TApply ty1 ty2, ty) -> unifyTypeApply (ty1, ty2) ty
  (ty, TApply ty1 ty2) -> unifyTypeApply (ty1, ty2) ty
  (ty, ty') ->
    lift . throwErr $ TypeUnificationError ty ty'
  where
    unifyTypeApply (tylam, typaram) ty = do
      (st, tyRes) <- collapseTypeApply tylam typaram
      unify (apply st tyRes) ty
    bindLookupName ::
      (IdentifierName a -> TI (Maybe TypeScheme)) ->
      (IdentifierName a -> CompilerError) ->
      IdentifierName a ->
      Type ->
      TI TypeSubst
    bindLookupName lookup' err name ty = do
      ty'M <- lookup' name
      case ty'M of
        Just tsch -> instantiate tsch >>= unify ty
        Nothing -> lift . throwErr $ err name
    bindTypeVar name = \case
      TVar n | n == name -> pure Map.empty
      ty
        | Set.member name (freeTypeVars ty) ->
          lift . throwErr $ OccursError name
      ty -> pure $ Map.singleton name ty

-- | Collapse type apply
collapseTypeApply :: Type -> Type -> TI (TypeSubst, Type)
collapseTypeApply (TName name) typaram = do
  tyschM <- lookupType name
  case tyschM of
    Just tysch ->
      instantiate tysch >>= \case
        TScope arg ty' -> pure (st, apply st ty')
          where
            st = Map.singleton arg typaram
        ty -> lift . throwErr $ KindMismatchError ty typaram
    Nothing -> lift . throwErr $ UnboundTypeError name
collapseTypeApply (TApply tylam' typaram') typaram =
  collapseTypeApply tylam' typaram' >>= uncurry resolveType
  where
    resolveType st (TScope arg ty') = pure (st', apply st' ty')
      where
        st' = Map.insert arg typaram st
    resolveType _ ty = lift . throwErr $ KindMismatchError ty typaram
collapseTypeApply ty typaram = lift . throwErr $ KindMismatchError ty typaram

-- | Replace all bound types with fresh polymorphic type vars
instantiate :: TypeScheme -> TI Type
instantiate (TypeScheme vars t) = do
  vars' <- mapM (\_ -> newTypeVar "a") vars
  pure $ apply (Map.fromList (zip vars vars')) t

-- | Generalize type over all free vars in type but not in type env
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t = flip TypeScheme t . Set.toList $ freeTypeVars t `Set.difference` freeTypeVars env

instance (TypeInference e) => TypeInference (Literal e) where
  infer = \case
    LiteralInt _ -> pure (Map.empty, TInt)
    LiteralString _ -> pure (Map.empty, TString)
    LiteralBool _ -> pure (Map.empty, TBool)
    LiteralFloat _ -> pure (Map.empty, TFloat)
    LiteralList exprs -> second TList <$> foldM mergeListTy (Map.empty, TUnknown) exprs
    LiteralTuple [] -> pure (Map.empty, TUnknown) -- TODO: Invalid case (maybe unit)
    LiteralTuple exprs -> second TTuple <$> foldM mergeTupleTy (Map.empty, []) exprs
    where
      mergeListTy = mergeExprTypes (\t1 t2 -> higherSp t1 t2 <$ unify t1 t2)
      mergeTupleTy = mergeExprTypes (\ts t -> pure $ ts ++ [t])
      mergeExprTypes merge (stAcc, tys) expr = withValues Map.empty $ do
        modifyEnv $ apply stAcc
        (st, ty) <- infer expr
        ty' <- merge tys ty
        pure (stAcc `composeSubst` st, ty')

instance TypeInference Expression where
  infer = \case
    Literal lit -> infer lit
    Lambda param body -> do
      typeVar <- newTypeVar "a"
      let newVal = Map.singleton param (TypeScheme [] typeVar)
      (bodySubst, bodyType) <- withValues newVal $ infer body
      pure (bodySubst, TLambda (apply bodySubst typeVar) bodyType)
    Apply lambda param -> do
      typeVar <- newTypeVar "a"
      (sl, tl) <- infer lambda
      modifyEnv $ apply sl -- NOTE: not sure if this should be scoped to the parameter
      (sp, tp) <- infer param
      sres <- unify (apply sp tl) (TLambda tp typeVar)
      pure (sres `composeSubst` sp `composeSubst` sl, apply sres typeVar)
    Var name -> do
      resMaybe <- lookupValue name
      case resMaybe of
        Nothing -> lift . throwErr $ UnboundVariableError name
        Just scheme -> (Map.empty,) <$> instantiate scheme
    Let bindings body -> do
      stDefs <- resolveDeclarationList bindings
      env <- getEnv
      (stBody, tyBody) <- withValues (envValues $ apply stDefs env) $ infer body
      pure (stDefs `composeSubst` stBody, tyBody)
    IfElse cond ifE elseE -> do
      (_, condT) <- infer cond
      unify condT TBool -- Check if condition is boolean
      (ifSt, ifT) <- infer ifE
      (elseSt, elseT) <- infer elseE
      subst <- unify ifT elseT
      let subst' = ifSt `Map.union` elseSt `Map.union` subst
      pure (subst', apply subst' $ higherSp ifT elseT)
    Ctor name ->
      lookupConstructor name >>= \case
        Just (Constructor ty _ []) -> pure (Map.empty, ty)
        Just (Constructor ty _ (tp : tps)) -> pure (Map.empty, foldl' TLambda tp tps `TLambda` ty)
        Nothing -> lift . throwErr $ UnboundConstructorError name
    CaseOf expr items -> do
      (stIn, tyIn) <- infer expr
      inTv <- newTypeVar "p"
      retTv <- newTypeVar "p"
      (stPat, tyPat, tyRet) <- foldM joinCase (Map.empty, inTv, retTv) items
      stIn' <- unify tyIn tyPat
      let st = stIn `composeSubst` stIn' `composeSubst` stPat
      pure (st, apply st tyRet)
    where
      joinCase :: (TypeSubst, Type, Type) -> CaseItem -> TI (TypeSubst, Type, Type)
      joinCase (st, tyI, tyR) item = do
        (st', tyI', tyR') <- inferCase item
        stI <- unify tyI' tyI
        stR <- unify tyR' tyR
        let subst = st `composeSubst` st' `composeSubst` stI `composeSubst` stR
        pure (subst, apply subst tyI, apply subst tyR)
      inferCase :: CaseItem -> TI (TypeSubst, Type, Type)
      inferCase (CaseItem pat guard expr) = withValues Map.empty $ do
        (stp, typ) <- infer pat
        (stg, tyg) <- infer guard
        unify tyg TBool -- Guard must be boolean
        (ste, tye) <- infer expr
        let subst = stp `composeSubst` stg `composeSubst` ste
        pure (subst, apply subst typ, apply subst tye)

instance TypeInference Pattern where
  infer = \case
    PatLiteral lit -> infer lit
    PatWildcard -> (Map.empty,) <$> newTypeVar "p"
    PatVar name -> do
      p <- newTypeVar "p"
      modifyEnv $ updateValues (Map.insert name $ TypeScheme [] p)
      pure (Map.empty, p)
    _ -> undefined

inferExpressionType :: Expression -> TI Type
inferExpressionType expr = uncurry apply <$> infer expr

-- | Check if type doesn't use kinds illegally
verifyValidKind :: Type -> TI ()
verifyValidKind ty@(TScope _ _) = lift . throwErr $ IllegalKindError ty
verifyValidKind (TLambda tyP tyB) = verifyValidKind tyP >> verifyValidKind tyB
verifyValidKind (TName name) =
  lookupType name
    >>= maybe
      (lift . throwErr $ UnboundTypeError name)
      (instantiate >=> verifyValidKind)
verifyValidKind _ = pure ()

verifyTypeVars :: Type -> TypeVars -> TI ()
verifyTypeVars (TScope v ty) vars = verifyTypeVars ty (Set.insert v vars)
verifyTypeVars typ vars = do
  if Set.null diff
    then pure ()
    else (lift . throwErr) (UnboundPolyTypeError $ Set.elemAt 0 diff)
  where
    diff = freeTypeVars typ `Set.difference` vars

resolveDeclaration :: TypeSubst -> Definition -> TI TypeSubst
resolveDeclaration st = \case
  (DefSignature name ty) -> do
    verifyValidKind ty
    let tsch = TypeScheme (Set.toList . freeTypeVars $ ty) ty
    modifyEnv $ updateValues (Map.insert name tsch)
    pure st
  (DefValue name expr) -> do
    -- Create a temporary scheme (needed for recursive definitions)
    typeDefn <- lookupValue name >>= maybe (pure TUnknown) instantiate

    env <- getEnv
    tmpTypeScheme <- generalize (apply st env) <$> newTypeVar "t"

    -- Infer type using tmpEnv
    (stBinding, properTypeScheme) <- withValues (Map.singleton name tmpTypeScheme) $ do
      (stBinding, tyBinding) <- infer expr
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
checkBlockType (Def def) = resolveDeclaration Map.empty def >>= (modifyEnv . apply)
checkBlockType (TypeDef name ty) = do
  verifyTypeVars ty Set.empty
  defineTypeAliases . Map.singleton name . TypeScheme (Set.toList $ freeTypeVars ty) $ ty
  flattenScope [] ty
  where
    flattenScope vars (TScope var ty') = flattenScope (var : vars) ty'
    flattenScope vars (TCtors ctors) =
      defineTypeConstructors . Map.fromList . map toCtor $ ctors
      where
        toCtor (Constructor _ n tps) = (n, Constructor cType n tps)
        cType = foldl' TApply (TName name) . map TVar $ vars
    flattenScope _ _ = pure ()

-- | Type check module
checkModule :: Module -> TI ()
checkModule (Module _ blocks) = mapM_ checkBlockType blocks

-- | Type check module and return env
checkModuleWithEnv :: Module -> TI TypeEnv
checkModuleWithEnv m = checkModule m >> getEnv

---
---
