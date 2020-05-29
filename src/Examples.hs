{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel

example_Sum =
  evalNamed $
    ("sum", "n" ==> CoreCase (Prim "Eq" [Var "n", Int 0]) [Prim "Plus" [Var "sum" @@ Prim "Minus" [Var "n", Int 1], Var "n"], Int 0]) `in_` Var "sum" @@ Int 1000000
