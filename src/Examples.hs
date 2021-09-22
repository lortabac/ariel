{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel.Core.Run
import Ariel.Core.Types

exampleSumr :: String
exampleSumr = evalCore exampleDefs (sumr @@ Int 100000)
  where
    sumr =
      Prim "fix"
        @@ ( "sum" ==> "n" ==> Prim "if" @@ (Prim "i=" @@ Var "n" @@ Int 0)
               @@ (Prim "i+" @@ Var "n" @@ (Var "sum" @@ (Prim "i-" @@ Var "n" @@ Int 1)))
               @@ Int 0
           )
