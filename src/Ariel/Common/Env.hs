{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ariel.Common.Env
  ( Env,
    emptyEnv,
    insertEnv,
    lookupEnv,
  )
where

import Ariel.Common.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Exts (IsList)

newtype Env a = Env (Map Name a) deriving (IsList)

emptyEnv :: Env a
emptyEnv = Env Map.empty

insertEnv :: Name -> a -> Env a -> Env a
insertEnv name def (Env env) = Env $ Map.insert name def env

lookupEnv :: Name -> Env a -> Maybe a
lookupEnv name (Env env) = Map.lookup name env
