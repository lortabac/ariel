{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ariel.TC.Types where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.TC.Constraints
import Ariel.TC.Context
import qualified Data.Set as Set
import Language.SexpGrammar (Position)
import Logic.Unify
import Validation

type TyCtx = Ctx Ty

type TyConstrs = Constraints Ty

newtype InferM a = InferM {unInferM :: UnifyT Ty (ReaderT TyCtx (State TyConstrs)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader TyCtx, MonadState TyConstrs)

instance MonadUnify Ty InferM where
  newVar p = InferM (newVar p)
  getUState = InferM getUState
  putUState = InferM . putUState
  lookupBinding = InferM . lookupBinding
  setBinding v = InferM . setBinding v
  unify' c h t1 t2 = InferM $ unify' c (\x y -> unInferM $ h x y) t1 t2
  applyBindings = InferM . applyBindings

runInferM :: InferM a -> (a, TyConstrs)
runInferM (InferM m) = runState (runReaderT (evalUnifyT m) emptyCtx) mempty

data TCMessage = TCMessage
  { position :: Position,
    tcError :: TCError
  }
  deriving (Eq, Show, Ord)

data TCError
  = TypeMismatch Ty Ty
  | SubsumptionError Ty Ty
  | OutOfScopeVar Name
  | OutOfScopeTyVar TyVar
  | CyclicType Ty
  | InvalidPrim Text
  | InvalidIOPrim Text
  | KindError Ty
  deriving (Eq, Show, Ord)

ok :: Applicative m => a -> m (Validation e a)
ok = pure . Success

ko :: Applicative m => e -> m (Validation (Set e) a)
ko err = pure $ Failure (Set.singleton err)

newMetavar :: InferM Ty
newMetavar = Metavar <$> newVar (Proxy :: Proxy Ty)

newBoundMetavar :: Ty -> InferM Ty
newBoundMetavar t = Metavar <$> newBoundVar t

addEqConstr :: Position -> Ty -> Ty -> InferM ()
addEqConstr p u1 u2 = InferM $ modify (\(Constraints cs) -> Constraints (EqConstr p u1 u2 : cs))
