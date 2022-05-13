{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Ariel.Common.IOPrim where

import Ariel.Prelude

data IOPrim a
  = Pure a
  | Display a
  | ReadLine
  deriving (Eq, Show, Functor, Foldable, Traversable)

readIOPrim :: Text -> [a] -> Maybe (IOPrim a)
readIOPrim "pure" [e] = Just $ Pure e
readIOPrim "read-line" [] = Just ReadLine
readIOPrim "display" [e] = Just $ Display e
readIOPrim _ _ = Nothing

readIOPrimOrDie :: Show a => Text -> [a] -> IOPrim a
readIOPrimOrDie name args = case readIOPrim name args of
  Just p -> p
  Nothing -> error ("Invalid io-prim: " <> show (name, args))
