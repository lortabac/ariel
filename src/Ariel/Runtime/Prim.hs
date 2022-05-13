{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Runtime.Prim where

import Ariel.Common.Prim
import Ariel.Runtime.Types
import GHC.Prim

evalPrim :: Prim Value -> Value
evalPrim (Eq (VInt x) (VInt y)) = VBool (x ==# y)
evalPrim (Eq (VString x) (VString y)) = VBool (boolToInt# (x == y))
evalPrim (Lt (VInt x) (VInt y)) = VBool (x <# y)
evalPrim (Plus (VInt x) (VInt y)) = VInt (x +# y)
evalPrim (Minus (VInt x) (VInt y)) = VInt (x -# y)
evalPrim p = error ("Invalid prim evaluation: " <> show p)
