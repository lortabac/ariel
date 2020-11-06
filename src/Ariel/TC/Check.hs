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
import Lens.Micro (over, (^.))

type TyTerm = UTerm Ty IntVar

type TyCtx = Ctx TyTerm

data TCError
  = InfiniteType IntVar TyTerm
  | TypeMismatch (Ty TyTerm) (Ty TyTerm)
  | SubsumptionError TyTerm TyTerm
  | InvalidApplicand TyTerm
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
  runIdentity $
    runIntBindingT $
      runExceptT (runWriterT (runReaderT m ctx))

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
  fst $
    runIdentity $
      resumeIntBindingT (runExceptT (runReaderT m ctx)) s

typecheck :: Expr -> Either TCError TyTerm
typecheck expr =
  case res of
    Left err -> throwError err
    Right (ty, constrs) -> runConstrM bindings $ do
      _ <- applyConstrs constrs
      ty' <- applyBindings ty
      generalize ty'
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
  ty <- infer e1
  case ty of
    UTerm (TArr s1 s2) -> do
      s1' <- generalize =<< infer e2
      -- FIXME freshening is expensive
      fs1 <- freshen s1
      fs1' <- freshen s1'
      dsk <- dskSubsumes fs1' fs1
      if dsk
        then do
          rh1 <- instantiate s1'
          rh2 <- instantiate s2
          addEqConstr ty (UTerm (TArr rh1 rh2))
          pure rh2
        else throwError (SubsumptionError s1' s1)
    t -> throwError (InvalidApplicand t)
infer (Let name e body) = do
  s <- generalize =<< infer e
  local (over localCtx (extendLocalCtx name s)) $ infer body
infer (Annot e ty) = do
  let s = unfreeze ty
  s' <- generalize =<< infer e
  -- FIXME freshening is expensive
  fs <- freshen s
  fs' <- freshen s'
  dsk <- dskSubsumes fs' fs
  if dsk
    then do
      rh <- instantiate s
      rh' <- instantiate s'
      addEqConstr rh rh'
      pure rh
    else throwError (SubsumptionError s' s)

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

dskSubsumes :: TyTerm -> TyTerm -> InferM Bool
dskSubsumes s1 s2 = do
  case weakPrenex s2 of
    UTerm (Forall vars rh) ->
      let (_, rh') = renameVarsIn (freeTyVars s1) vars rh
       in dskSubsumes_ s1 rh'
    _ -> pure True

dskSubsumes_ :: TyTerm -> TyTerm -> InferM Bool
dskSubsumes_ (UTerm (TArr s1 s2)) (UTerm (TArr s3 s4)) =
  (&&) <$> dskSubsumes s3 s1 <*> dskSubsumes_ s2 s4
dskSubsumes_ s1 s2 = do
  s1' <- instantiate s1
  s2' <- instantiate s2
  subsumes s1' s2'

weakPrenex :: TyTerm -> TyTerm
weakPrenex (UTerm TInt) = UTerm TInt
weakPrenex (UTerm TDouble) = UTerm TDouble
weakPrenex (UTerm TText) = UTerm TText
weakPrenex (UTerm (TVar v)) = UTerm (TVar v)
weakPrenex (UVar v) = UVar v
weakPrenex t@(UTerm (TArr s1 s2)) = case weakPrenex s2 of
  UTerm (Forall tyVars r2) ->
    let (tyVars', r2') = renameVarsIn (freeTyVars s1) tyVars r2
     in UTerm (Forall tyVars' (UTerm (TArr s1 r2')))
  _ -> t
weakPrenex t@(UTerm (Forall tyVars r1)) = case weakPrenex r1 of
  UTerm (Forall tyVars' r2) ->
    let (renamedTyVars, r2') = renameVarsIn tyVars tyVars' r2
     in UTerm (Forall (tyVars ++ renamedTyVars) r2')
  _ -> t

renameVarsIn :: [TyVar] -> [TyVar] -> TyTerm -> ([TyVar], TyTerm)
renameVarsIn taken new t = (renamed, t')
  where
    (renamed, substs) = freshenTyVars taken new
    t' = applySubsts substs t

freshenTyVars :: [TyVar] -> [TyVar] -> ([TyVar], Map TyVar TyVar)
freshenTyVars taken = foldr freshenTyVar ([], Map.empty)
  where
    freshenTyVar tv@(TyVar v) (vars, subs) =
      if tv `elem` taken
        then
          let tyVar' = TyVar (v ++ "#")
           in (tyVar' : vars, Map.insert (TyVar v) tyVar' subs)
        else (tv : vars, subs)

freeTyVars :: TyTerm -> [TyVar]
freeTyVars = go []
  where
    go _ (UTerm TInt) = []
    go _ (UTerm TText) = []
    go _ (UTerm TDouble) = []
    go taken (UTerm (TArr ty1 ty2)) = go taken ty1 ++ go taken ty2
    go taken (UTerm (TVar tv)) = if tv `elem` taken then [] else [tv]
    go taken (UTerm (Forall vars t)) = go (vars ++ taken) t
    go _ (UVar _) = []

applySubsts :: Map TyVar TyVar -> TyTerm -> TyTerm
applySubsts subts = go
  where
    go (UTerm TInt) = UTerm TInt
    go (UTerm TText) = UTerm TText
    go (UTerm TDouble) = UTerm TDouble
    go (UTerm (TArr ty1 ty2)) = UTerm (TArr (go ty1) (go ty2))
    go t@(UTerm (TVar tv)) = case Map.lookup tv subts of
      Just tv' -> UTerm (TVar tv')
      Nothing -> t
    go (UTerm (Forall vars t)) = UTerm (Forall vars (go t))
    go (UVar v) = UVar v

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
