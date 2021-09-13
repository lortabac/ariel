{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Common.IOPrim where

import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic
import Control.DeepSeq

data IOPrim e
  = WriteLn e
  | ReadLine
  deriving (Eq, Show, Functor, Generic)

instance NFData e => NFData (IOPrim e)

instance SexpIso e => SexpIso (IOPrim e) where
  sexpIso =
    match $
      With (list (el (hashed (sym "write-ln")) >>> el sexpIso) >>>) $
        With
          (hashed (sym "read-line") >>>)
          End
