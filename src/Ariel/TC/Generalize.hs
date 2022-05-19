module Ariel.TC.Generalize where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.TC.Constraints
import Ariel.TC.Context
import Ariel.TC.Types
import Control.Lens (transform)
import Data.List (nub, (\\))
import Language.Sexp.Located (dummyPos)
import Logic.Unify
import Validation

generalize :: Ty -> InferM (Validation (Set TCMessage) Ty)
generalize t = do
  tFreeVars <- fmap nub <$> getFreeVars t
  gFreeVars <- fmap nub ctxFreeVars
  let intVars = fmap (\\ gFreeVars) tFreeVars
  case intVars of
    Success [] -> ok t
    Success metavars -> do
      let tyVarMap = zip metavars tyVarSupply
          t' = foldl (\acc (v, tv) -> subst v tv acc) t tyVarMap
      t'' <- applyBindingsV t'
      let tyVars = nub $ map snd tyVarMap
      pure $ Forall tyVars <$> t''
    Failure e -> pure $ Failure e

subst :: UVar -> TyVar -> Ty -> Ty
subst v tvar = transform go
  where
    go (Metavar v')
      | v == v' = TVar tvar
      | otherwise = Metavar v'
    go t = t

ctxFreeVars :: InferM [UVar]
ctxFreeVars = asks (concatMap getVars . localCtxElems . _localCtx)

applyBindingsV :: Ty -> InferM (Validation (Set TCMessage) Ty)
applyBindingsV t = do
  maybeT <- applyBindings t
  case maybeT of
    Just t' -> ok t'
    Nothing -> ko $ TCMessage dummyPos (CyclicType t)

getFreeVars :: Ty -> InferM (Validation (Set TCMessage) [UVar])
getFreeVars t = do
  res <- applyConstraints
  validation (pure . Failure) (const . ok $ getVars t) res

applyConstraints :: InferM (Validation (Set TCMessage) ())
applyConstraints = do
  eqs <- gets eqConstrs
  ress <- for eqs $ \(EqConstr p u1 u2) -> do
    res <- unify u1 u2
    case res of
      Unified -> ok ()
      OccursFailure _ t -> ko $ TCMessage p (CyclicType t)
      UnificationFailure t1 t2 -> ko $ TCMessage p (TypeMismatch t1 t2)
      SubsumptionFailure t1 t2 -> ko $ TCMessage p (SubsumptionError t1 t2)
  put mempty
  pure $ () <$ sequenceA ress
