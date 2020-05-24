{-# LANGUAGE OverloadedLists #-}

module Ariel.Evaluation.Prim where

import Ariel.Evaluation.Types

evalPrim :: Prim -> [Expr] -> Expr
evalPrim Eq [x, y] = if x == y then Cons 1 [] else Cons 0 []
evalPrim Plus [Int x, Int y] = Int (x + y)
evalPrim Minus [Int x, Int y] = Int (x - y)
evalPrim ConcatText [Text x, Text y] = Text (x <> y)
evalPrim p args = error ("Invalid Prim: " <> show (p, args))
