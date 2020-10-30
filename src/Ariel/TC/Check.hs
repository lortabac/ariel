{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ariel.TC.Check where

import Ariel.Syntax.AST
import Ariel.Syntax.Ty
import Ariel.Syntax.Types
import Ariel.TC.Constraints
import Ariel.TC.Context
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Unification
import Control.Unification.IntVar
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.List ((\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.), over)

type TyTerm = UTerm Ty IntVar

type TyCtx = Ctx TyTerm

data TCError
  = InfiniteType IntVar TyTerm
  | TypeMismatch (Ty TyTerm) (Ty TyTerm)
  | NotInScope Name
  | GlobalNotInScope GName
  | TyVarNotInScope TyVar
  | ImpossibleHappened String
  deriving (Show)

instance Fallible Ty IntVar TCError where

  occursFailure v t = InfiniteType v t

  mismatchFailure t1 t2 = TypeMismatch t1 t2

type TyConstrs = Constraints TyTerm

newtype InferM a = InferM (ReaderT TyCtx (WriterT TyConstrs (ExceptT TCError (IntBindingT Ty Identity))) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Ctx TyTerm),
      MonadWriter (Constraints TyTerm),
      MonadError TCError
    )

instance BindingMonad Ty IntVar InferM where

  lookupVar = InferM . lift . lift . lift . lookupVar

  freeVar = InferM . lift . lift . lift $ freeVar

  newVar = InferM . lift . lift . lift . newVar

  bindVar v = InferM . lift . lift . lift . bindVar v

runInferM :: InferM a -> (Either TCError (a, TyConstrs), IntBindingState Ty)
runInferM = runInferMWithCtx emptyCtx

runInferMWithCtx ::
  TyCtx ->
  InferM a ->
  (Either TCError (a, TyConstrs), IntBindingState Ty)
runInferMWithCtx ctx (InferM m) =
  runIdentity
    $ runIntBindingT
    $ runExceptT (runWriterT (runReaderT m ctx))

newtype ConstrM a = ConstrM (ReaderT TyCtx (ExceptT TCError (IntBindingT Ty Identity)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Ctx TyTerm),
      MonadError TCError
    )

instance BindingMonad Ty IntVar ConstrM where

  lookupVar = ConstrM . lift . lift . lookupVar

  freeVar = ConstrM . lift . lift $ freeVar

  newVar = ConstrM . lift . lift . newVar

  bindVar v = ConstrM . lift . lift . bindVar v

runConstrM :: IntBindingState Ty -> ConstrM a -> Either TCError a
runConstrM = runConstrMWithCtx emptyCtx

runConstrMWithCtx ::
  Ctx TyTerm ->
  IntBindingState Ty ->
  ConstrM a ->
  Either TCError a
runConstrMWithCtx ctx s (ConstrM m) =
  fst $ runIdentity $
    resumeIntBindingT (runExceptT (runReaderT m ctx)) s

typecheck :: Expr -> Either TCError TyTerm
typecheck expr =
  case res of
    Left err -> throwError err
    Right (ty, constrs) -> runConstrM bindings $ do
      _ <- applyConstrs constrs
      generalize =<< applyBindings ty
  where
    (res, bindings) = runInferM $ infer expr

infer :: Expr -> InferM TyTerm
infer Int {} = pure $ UTerm TInt
infer Double {} = pure $ UTerm TDouble
infer Text {} = pure $ UTerm TText
infer (Var name) = do
  ctx <- ask
  case lookupLocalCtx name (ctx ^. localCtx) of
    Just t -> instantiate t
    Nothing -> throwError (NotInScope name)
infer (QVar name) = do
  ctx <- ask
  case lookupGlobalCtx (GName name) (ctx ^. globalCtx) of
    Just t -> instantiate t
    Nothing -> throwError (GlobalNotInScope (GName name))
infer (Lam name e) =
  do
    varTy <- freeUVar
    bodyTy <- local (over localCtx (extendLocalCtx name varTy)) $ infer e
    pure $ UTerm (TArr varTy bodyTy)
infer (App e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2
  appTy <- freeUVar
  addEqConstr ty1 (UTerm (TArr ty2 appTy))
  pure appTy
infer (Let name e body) = do
  s <- generalize =<< infer e
  local (over localCtx (extendLocalCtx name s)) $ infer body

instantiate :: TyTerm -> InferM TyTerm
instantiate (UTerm (Forall vars e)) = do
  metaVars <- traverse (const freeUVar) vars
  let metaMap = Map.fromList $ zip vars metaVars
  instantiate' metaMap e
instantiate e = pure e

instantiate' :: Map TyVar TyTerm -> TyTerm -> InferM TyTerm
instantiate' metaMap t = go t
  where
    go (UTerm TInt) = pure $ UTerm TInt
    go (UTerm TDouble) = pure $ UTerm TDouble
    go (UTerm TText) = pure $ UTerm TText
    go (UTerm (TArr ty1 ty2)) = (\t1 t2 -> UTerm (TArr t1 t2)) <$> go ty1 <*> go ty2
    go (UTerm (TVar v)) = case Map.lookup v metaMap of
      Just metaVar -> pure metaVar
      Nothing -> throwError (TyVarNotInScope v)
    go (UTerm (Forall vars e)) = pure $ UTerm (Forall vars e)
    go (UVar v) = pure $ UVar v

generalize ::
  (MonadError TCError m, BindingMonad Ty IntVar m, MonadReader (Ctx TyTerm) m) =>
  TyTerm ->
  m TyTerm
generalize t = do
  tFreeVars <- getFreeVars t
  gFreeVars <- ctxFreeVars
  let intVars = tFreeVars \\ gFreeVars
  case intVars of
    [] -> pure t
    intVars' -> do
      let tVars = fmap UVar intVars'
          tVarMap = zip tVars tVarSupply
      _ <- traverse_ (uncurry unify) tVarMap
      t' <- applyBindings t
      tVars' <- applyBindingsAll tVars
      tyVars <- traverse getTyVar tVars'
      pure $ UTerm (Forall tyVars t')

getTyVar :: MonadError TCError m => TyTerm -> m TyVar
getTyVar (UTerm (TVar v)) = pure v
getTyVar t = throwError $ ImpossibleHappened ("Expected TVar but found " <> show t)

weakPrenex :: MonadError TCError m => TyTerm -> m TyTerm
weakPrenex = go
  where
    go (UTerm TInt) = pure $ UTerm TInt
    go (UTerm TDouble) = pure $ UTerm TDouble
    go (UTerm TText) = pure $ UTerm TText
    go (UTerm (TVar v)) = pure $ UTerm (TVar v)
    go (UVar v) = pure $ UVar v
    go t@(UTerm (TArr s1 s2)) = do
      pr2 <- go s2
      case pr2 of
        UTerm (Forall tyVars r2) ->
          pure $ UTerm (Forall tyVars (UTerm (TArr s1 r2)))
        _ -> pure t
    go t@(UTerm (Forall tyVars r1)) = do
      pr1 <- go r1
      case pr1 of
        UTerm (Forall tyVars' r2) -> do
          let (renamedTyVars, substs) = freshenTyVars tyVars tyVars'
          r2' <- applySubsts substs r2
          pure $ UTerm (Forall (tyVars ++ renamedTyVars) r2')
        _ -> pure t

freshenTyVars :: [TyVar] -> [TyVar] -> ([TyVar], Map TyVar TyVar)
freshenTyVars taken = foldr freshenTyVar ([], Map.empty)
  where
    freshenTyVar tv@(TyVar v) (vars, subs) =
      if tv `elem` taken
        then
          let tyVar' = TyVar (v ++ "#")
           in (tyVar' : vars, Map.insert (TyVar v) tyVar' subs)
        else (tv : vars, subs)

applySubsts :: MonadError TCError m => Map TyVar TyVar -> TyTerm -> m TyTerm
applySubsts subts = go
  where
    go (UTerm TInt) = pure $ UTerm TInt
    go (UTerm TText) = pure $ UTerm TText
    go (UTerm TDouble) = pure $ UTerm TDouble
    go (UTerm (TArr ty1 ty2)) = (\t1 t2 -> UTerm (TArr t1 t2)) <$> go ty1 <*> go ty2
    go (UTerm (TVar tv)) = case Map.lookup tv subts of
      Just tv' -> pure $ UTerm (TVar tv')
      Nothing -> throwError $ ImpossibleHappened ("TyVar " <> show tv <> " is not in the substitution map")
    go (UTerm (Forall vars t)) = UTerm . Forall vars <$> go t
    go (UVar v) = pure $ UVar v

ctxFreeVars ::
  (MonadError TCError m, BindingMonad Ty IntVar m, MonadReader (Ctx TyTerm) m) =>
  m [IntVar]
ctxFreeVars = getFreeVarsAll =<< localCtxElems <$> asks _localCtx

tVarSupply :: [TyTerm]
tVarSupply = UTerm . TVar <$> tyVarSupply

applyConstrs :: TyConstrs -> ConstrM ()
applyConstrs = applyEqConstrs . eqConstrs

applyEqConstrs :: [(TyTerm, TyTerm)] -> ConstrM ()
applyEqConstrs constrs = traverse_ applyEqConstr constrs
  where
    applyEqConstr (t1, t2) = unify t1 t2

freeUVar :: InferM TyTerm
freeUVar = fmap UVar freeVar

newUVar :: TyTerm -> InferM TyTerm
newUVar t = fmap UVar (newVar t)
