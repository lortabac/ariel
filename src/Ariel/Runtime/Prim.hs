module Ariel.Runtime.Prim where

import Ariel.Common.Prim
import Ariel.Runtime.Types

evalPrim2 :: Prim2 -> Expr -> Expr -> Expr
evalPrim2 IntPlus (Int m) (Int n) = Int (m + n)
evalPrim2 IntMinus (Int m) (Int n) = Int (m - n)
evalPrim2 IntTimes (Int m) (Int n) = Int (m * n)
evalPrim2 IntEq (Int m) (Int n) = if m == n then true else false
evalPrim2 _ _ _ = error "Invalid prim2"
