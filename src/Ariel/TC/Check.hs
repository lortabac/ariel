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
import Data.Functor.Identity

type TyTerm = UTerm Ty IntVar

type TyCtx = Ctx TyTerm

data TCError
  = InfiniteType IntVar TyTerm
  | TypeMismatch (Ty TyTerm) (Ty TyTerm)
  | NotInScope Name
  deriving (Show)

instance Fallible Ty IntVar TCError where
  occursFailure v t = InfiniteType v t

  mismatchFailure t1 t2 = TypeMismatch t1 t2

type CheckM a = ReaderT TyCtx (WriterT TyConstrs (ExceptT TCError (IntBindingT Ty Identity))) a

type TyConstrs = Constraints TyTerm

runCheckM :: CheckM a -> (Either TCError (a, TyConstrs), IntBindingState Ty)
runCheckM = runCheckMWithCtx emptyCtx

runCheckMWithCtx :: TyCtx -> CheckM a -> (Either TCError (a, TyConstrs), IntBindingState Ty)
runCheckMWithCtx ctx m = runIdentity $ runIntBindingT $ runExceptT (runWriterT (runReaderT m ctx))

runInferWithCtx :: TyCtx -> CheckM a -> Either TCError a
runInferWithCtx ctx m = fst <$> fst (runCheckMWithCtx ctx m)

runInfer :: CheckM a -> Either TCError a
runInfer m = fst <$> fst (runCheckM m)

infer :: Expr -> CheckM TyTerm
infer Int {} = pure $ UTerm TInt
infer Double {} = pure $ UTerm TDouble
infer Text {} = pure $ UTerm TText
infer (Var name) = do
  ctx <- ask
  case lookupCtx name ctx of
    Just t -> pure t
    Nothing -> throwError (NotInScope name)
infer (Lam name e) =
  do
    varTy <- freeUVar
    bodyTy <- local (extendCtx name varTy) $ infer e
    pure $ UTerm (TArr varTy bodyTy)
infer (App e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2
  appTy <- freeUVar
  addEqConstr ty1 (UTerm (TArr ty2 appTy))
  pure appTy

freeUVar :: CheckM TyTerm
freeUVar = lift . lift . lift $ fmap UVar freeVar

newUVar :: TyTerm -> CheckM TyTerm
newUVar t = lift . lift . lift $ fmap UVar (newVar t)
