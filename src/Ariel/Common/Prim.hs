{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Ariel.Common.Prim where

import Ariel.Prelude

data Prim a
  = Eq a a
  | Lt a a
  | Plus a a
  | Minus a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

readPrim :: Text -> [a] -> Maybe (Prim a)
readPrim "=" [e1, e2] = Just $ Eq e1 e2
readPrim "<" [e1, e2] = Just $ Lt e1 e2
readPrim "+" [e1, e2] = Just $ Plus e1 e2
readPrim "-" [e1, e2] = Just $ Minus e1 e2
readPrim _ _ = Nothing

readPrimOrDie :: Show a => Text -> [a] -> Prim a
readPrimOrDie name args = case readPrim name args of
  Just p -> p
  Nothing -> error ("Invalid prim: " <> show (name, args))
