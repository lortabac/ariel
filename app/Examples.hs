{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel.Common.Types
import Ariel.Core.Eval
import Ariel.Core.Types

defs :: Defs
defs =
  Defs
    { globals = mempty,
      sumTypes =
        [ (QName "base" "bool", [("false", []), ("true", [])]),
        ]
    }

exampleSumr :: IO Expr
exampleSumr = runCore defs (sumr @@ Int 10000000)
  where
    sumr =
      Prim "fix"
        @@ ( "sum" ==> "n"
               ==> Case
                 (Prim "i=" @@ Var "n" @@ Int 0)
                 [ ("false", Prim "i+" @@ Var "n" @@ (Var "sum" @@ (Prim "i-" @@ Var "n" @@ Int 1))),
                   ("true", Int 0)
                 ]
           )

exampleFib :: IO Expr
exampleFib = runCore defs (fib @@ Int 40)
  where
    fib =
      Prim "fix"
        @@ ( "fib" ==> "n"
               ==> Case
                 (Prim "i<" @@ Var "n" @@ Int 2)
                 [ ("true", Var "n"),
                   ("false", Prim "i+" @@ (Var "fib" @@ (Prim "i-" @@ Var "n" @@ Int 1)) @@ (Var "fib" @@ (Prim "i-" @@ Var "n" @@ Int 2)))
                 ]
           )

exampleFibw :: IO Expr
exampleFibw = runCore defs (fib @@ Int 35)
  where
    fib =
        ("x" ==> Var "x" @@ Var "x")
        @@ ( "fib" ==> "n"
               ==> Case
                 (Prim "i<" @@ Var "n" @@ Int 2)
                 [ ("true", Int 1),
                   ("false", Prim "i+" @@ (Var "fib" @@ Var "fib" @@ (Prim "i-" @@ Var "n" @@ Int 1)) @@ (Var "fib" @@ Var "fib" @@ (Prim "i-" @@ Var "n" @@ Int 2)))
                 ]
           )
