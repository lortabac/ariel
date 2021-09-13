module Ariel.Core.Run where

import Ariel.Core.Compile
import Ariel.Core.Types
import Ariel.Runtime.Eval (eval)
import Ariel.Runtime.IO (run)

runCore :: Defs -> Expr -> IO Expr
runCore defs expr = decompileRT <$> run rtGlobals rtExpr
  where
    (rtExpr, rtGlobals) = compileCore defs expr

evalCore :: Defs -> Expr -> IO Expr
evalCore defs expr = decompileRT <$> eval rtGlobals rtExpr
  where
    (rtExpr, rtGlobals) = compileCore defs expr
