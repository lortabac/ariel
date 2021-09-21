{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ariel.Core.Run where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Runtime.Eval (eval)
import Ariel.Runtime.Compile
import Ariel.Runtime.Purify

evalCore :: Defs -> Expr -> String
evalCore defs expr = show $ eval mGlobals mExpr
  where
    (pExpr, pGlobals) = purify defs expr
    mGlobals = fmap compile pGlobals
    mExpr = compile pExpr

exampleDefs :: Defs
exampleDefs = Defs
    { globals = mempty
    , sumTypes = [(QName "base" "bool", ["false", "true"])
                 ,(QName "base" "maybe", ["nothing", "just"])
                 ,(QName "base" "list", ["nil", "cons"])
                 ]
    }

nothing :: Expr
nothing = Con (QName "base" "maybe") (Tag "nothing") []

just :: Expr
just = Con (QName "base" "maybe") (Tag "just") [Var "x"]

false :: Expr
false = Con (QName "base" "bool") (Tag "false") []

true :: Expr
true = Con (QName "base" "bool") (Tag "true") []

nil :: Expr
nil = Con (QName "base" "list") (Tag "nil") []

cons :: Expr
cons = Con (QName "base" "list") (Tag "cons") [Var "x", Var "y"]

head_ :: Expr
head_ = "xs" ==> Case (Var "xs") [("nil", Int 0), ("cons", "y" ==> "ys" ==> Var "y")]

tail_ :: Expr
tail_ = "xs" ==> Case (Var "xs") [("nil", Int 0), ("cons", "y" ==> "ys" ==> Var "ys")]
