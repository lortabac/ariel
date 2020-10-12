{-# LANGUAGE FlexibleContexts #-}

module Ariel.TC.Constraints where

import Control.Monad.Writer

data Constraints a = Constraints {eqConstrs :: [(a, a)]}
  deriving (Eq, Show)

instance Semigroup (Constraints a) where
  Constraints eq1 <> Constraints eq2 = Constraints (eq1 <> eq2)

instance Monoid (Constraints a) where
  mempty = Constraints mempty

addEqConstr :: MonadWriter (Constraints a) m => a -> a -> m ()
addEqConstr u1 u2 = tell $ Constraints [(u1, u2)]
