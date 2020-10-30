{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ariel.Syntax.Ty where

import Ariel.Syntax.Types
import Control.Unification (Unifiable)
import GHC.Generics

data Ty a
  = TInt
  | TDouble
  | TText
  | TArr a a
  | TVar TyVar
  | Forall [TyVar] a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance Unifiable Ty

-- instance Unifiable ((,) Tag)

instance Unifiable []
