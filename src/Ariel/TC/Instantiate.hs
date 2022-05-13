module Ariel.TC.Instantiate where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.TC.Types
import qualified Data.Map as Map
import Validation

instantiate :: Ty -> InferM (Validation (Set TCError) Ty)
instantiate (Forall vars t) = do
  metavars <- traverse (const newMetavar) vars
  let metaMap = Map.fromList $ zip vars metavars
  instantiate' metaMap t
instantiate t = ok t

instantiate' :: Map TyVar Ty -> Ty -> InferM (Validation (Set TCError) Ty)
instantiate' metaMap = go
  where
    go t@TCon {} = ok t
    go (TApp t1 t2) = do
      t1' <- go t1
      t2' <- go t2
      pure $ TApp <$> t1' <*> t2'
    go (TVar v) = case Map.lookup v metaMap of
      Just metavar -> ok metavar
      Nothing -> ko $ OutOfScopeTyVar v
    go (Forall vars t) = ok $ Forall vars t
    go (Metavar v) = ok $ Metavar v
