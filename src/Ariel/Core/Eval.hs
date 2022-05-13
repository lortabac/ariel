{-# LANGUAGE MagicHash #-}

module Ariel.Core.Eval where

import Ariel.Core.Compile
import Ariel.Core.Types
import Ariel.Runtime.Eval (eval, run)
import Ariel.Runtime.Nameless
import Ariel.Runtime.Types
import GHC.Types (Int (..))

runCore :: Defs -> Expr -> IO Expr
runCore defs expr = do
  readBack <$> run iGlobals mempty iExpr
  where
    (pExpr, pGlobals) = compile defs expr
    iGlobals = fmap removeNames pGlobals
    iExpr = removeNames pExpr

evalCore :: Defs -> Expr -> Expr
evalCore defs expr = readBack $ eval iGlobals mempty iExpr
  where
    (pExpr, pGlobals) = compile defs expr
    iGlobals = fmap removeNames pGlobals
    iExpr = removeNames pExpr

readBack :: Value -> Expr
readBack (VInt i) = Int (I# i)
readBack (VString s) = String s
readBack v = error ("Cannot read back to Ariel: " <> show v)
