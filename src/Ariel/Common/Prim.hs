{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Ariel.Common.Prim where

import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

data Prim2 = IntPlus
           | IntMinus
           | IntTimes
           | IntEq
           deriving (Eq, Show, Generic)

instance SexpIso Prim2 where
    sexpIso = match
        $ With (sym "int-plus" >>>)
        $ With (sym "int-minus" >>>)
        $ With (sym "int-times" >>>)
        $ With (sym "int-eq" >>>)
        End
