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
    , sumTypes = [(QName "base" "bool", ["false", "true"])]
    }
