{-# LANGUAGE DataKinds #-}

module Ariel.Evaluation.IO
  ( run,
    runNamed,
  )
where

import Ariel.Evaluation.Eval
import Ariel.Evaluation.IOPrim
import Ariel.Evaluation.Nameless
import Ariel.Evaluation.Types
import qualified Ariel.Syntax.AST as AST
import Ariel.Syntax.Canonical

runNamed :: AST.Expr -> IO Expr
runNamed e = do
  env <- emptyEnv
  run env (removeNames (makeLetRecs e))

run :: Env -> Expr -> IO Expr
run env (Pure e) = eval env e
run env (IOPrim p args) = runIOPrim p =<< traverse (eval env) args
run env (Bind (IOPrim p args) (Abs t2)) = do
  t1 <- runIOPrim p =<< traverse (eval env) args
  (env', e) <- evalApp env (App (Abs t2) t1)
  run env' e
run env (Bind (Pure t1) (Abs t2)) = do
  t1' <- eval env t1
  (env', e) <- evalApp env (App (Abs t2) t1')
  run env' e
run _ e@(Bind _ _) = error ("Invalid Bind: " <> show e)
run env e = run env =<< eval env e
