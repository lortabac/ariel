{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel.Common.Types
import Ariel.Core.Run
import Ariel.Core.Types

defs :: Defs
defs =
  Defs
    { globals = mempty,
      sumTypes =
        [ (QName "base" "bool", [("false", 0), ("true", 0)]),
          (QName "base" "maybe", [("nothing", 0), ("just", 1)])
        ]
    }

exampleSumr :: String
exampleSumr = evalCore defs (sumr @@ Int 10000000)
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

exampleFib :: String
exampleFib = evalCore defs (fib @@ Int 40)
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
