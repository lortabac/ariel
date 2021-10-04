module Ariel.Core.Eval where

import Ariel.Core.Types
import Ariel.Runtime.Compile
import Ariel.Runtime.Eval (eval, run)
import Ariel.Runtime.Purify
import Ariel.Runtime.Types

runCore :: Defs -> Expr -> IO Expr
runCore defs expr = readback <$> run mGlobals mExpr
  where
    (pExpr, pGlobals) = purify defs expr
    mGlobals = fmap compile pGlobals
    mExpr = compile pExpr

evalCore :: Defs -> Expr -> Expr
evalCore defs expr = readback $ eval mGlobals mExpr
  where
    (pExpr, pGlobals) = purify defs expr
    mGlobals = fmap compile pGlobals
    mExpr = compile pExpr

readback :: Value -> Expr
readback (VInt i) = Int i
readback (VString s) = String s
readback _ = error "Can't read back to Core"
