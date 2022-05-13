{-# LANGUAGE FlexibleContexts #-}

module Ariel.TC.Constraints where

data Constraints a = Constraints {eqConstrs :: [(a, a)]}
  deriving (Eq, Show)

instance Semigroup (Constraints a) where
  Constraints eq1 <> Constraints eq2 = Constraints (eq1 <> eq2)

instance Monoid (Constraints a) where
  mempty = Constraints mempty
