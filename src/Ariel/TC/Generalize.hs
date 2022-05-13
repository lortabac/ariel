module Ariel.TC.Generalize where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.TC.Constraints
import Ariel.TC.Context
import Ariel.TC.Types
import Data.List (nub, (\\))
import Logic.Unify
import Validation

generalize :: Ty -> InferM (Validation (Set TCError) Ty)
generalize t = do
  tFreeVars <- getFreeVars t
  gFreeVars <- ctxFreeVars
  let intVars = fmap (\\ gFreeVars) tFreeVars
  case intVars of
    Success [] -> ok t
    Success metavars -> do
      let vars = map Metavar metavars
          letters = map TVar tyVarSupply
          tyVarMap = zip vars letters
      traverse_ (uncurry unify) tyVarMap
      t' <- applyBindingsV t
      metavars' <- traverse applyBindingsV vars
      let tyVars = nub $ map (fmap getTyVar) metavars'
      case failures tyVars of
        [] -> pure $ Forall (successes tyVars) <$> t'
        errs -> pure $ Failure (mconcat errs)
    Failure e -> pure $ Failure e

ctxFreeVars :: InferM [UVar]
ctxFreeVars = asks (concatMap getVars . localCtxElems . _localCtx)

getTyVar :: Ty -> TyVar
getTyVar (TVar v) = v
getTyVar _ = error "The impossible happened"

applyBindingsV :: Ty -> InferM (Validation (Set TCError) Ty)
applyBindingsV t = do
  maybeT <- applyBindings t
  case maybeT of
    Just t' -> ok t'
    Nothing -> ko $ CyclicType t

getFreeVars :: Ty -> InferM (Validation (Set TCError) [UVar])
getFreeVars t = do
  res <- applyConstraints
  validation (pure . Failure) (const . ok $ getVars t) res

applyConstraints :: InferM (Validation (Set TCError) ())
applyConstraints = do
  eqs <- gets eqConstrs
  res <- mconcat <$> traverse (uncurry unify) eqs
  case res of
    Unified -> do
      put $ Constraints []
      ok ()
    OccursFailure _ t -> ko $ CyclicType t
    UnificationFailure t1 t2 -> ko $ TypeMismatch t1 t2
    SubsumptionFailure t1 t2 -> ko $ SubsumptionError t1 t2
