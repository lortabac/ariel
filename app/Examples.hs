{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel

example_Sum =
  evalNamed $
    ("sum", "n" ==> CoreCase (Prim2 "Eq" (Var "n") (Int 0)) [Prim2 "Plus" (Var "sum" @@ Prim2 "Minus" (Var "n") (Int 1)) (Var "n"), Int 0]) `inrec` Var "sum" @@ Int 10000000
