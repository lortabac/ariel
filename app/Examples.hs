{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples where

import Ariel.Common.Types
import Ariel.Core.Run
import Ariel.Core.Types

defs :: Defs
defs = Defs
    { globals = mempty
    , sumTypes = [(QName "base" "bool", ["false", "true"])
                 ,(QName "base" "maybe", ["nothing", "just"])
                 ]
    }

exampleSumr :: String
exampleSumr = evalCore defs (sumr @@ Int 10000000)
  where
    sumr = Prim "fix" @@ ("sum" ==> "n" ==> Case (Prim "i=" @@ Var "n" @@ Int 0)
        [ ("true", Prim "i+" @@ Var "n" @@ (Var "sum" @@ (Prim "i-" @@ Var "n" @@ Int 1)))
        , ("false", Int 0)
        ])
