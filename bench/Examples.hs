{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel.Common.Prim
import Ariel.Common.Types
import Ariel.Core.Run
import Ariel.Core.Types

exampleDefs :: Defs
exampleDefs =
  Defs
    { globals = mempty,
      sumTypes = [(QName "base" "bool", ["false", "true"])]
    }

exampleSum :: Int -> IO Expr
exampleSum n = evalCore exampleDefs (sumNumbers @@ [Int n])
  where
    sumNumbers =
      Fix "sum" $
        ["n"]
          ==> Case
            (Prim2 IntEq (Var "n") (Int 0))
            [ ("false", Prim2 IntPlus (Var "n") (Var "sum" @@ [Prim2 IntMinus (Var "n") (Int 1)])),
              ("true", Int 0)
            ]
