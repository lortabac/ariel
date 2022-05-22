{-# LANGUAGE OverloadedStrings #-}

module Ariel.TC.Kind where

import Ariel.Core.Types

kindCheck :: Ty -> Maybe Kind
kindCheck (TCon "Int") = Just Star
kindCheck (TCon "String") = Just Star
kindCheck (TCon "Bool") = Just Star
kindCheck (TCon "IO") = Just $ KArr Star Star
kindCheck (TCon _) = Nothing
kindCheck (TArr t1 t2) = (\_ _ -> Star) <$> kindCheck t1 <*> kindCheck t2
kindCheck (TApp t1 t2) = case (kindCheck t1, kindCheck t2) of
  (Just (KArr k1 k2), Just l1)
    | k1 == l1 -> Just k2
    | otherwise -> Nothing
  _ -> Nothing
kindCheck (TVar _) = Just Star
kindCheck (Forall _ t) = kindCheck t
kindCheck (Metavar _) = Nothing
