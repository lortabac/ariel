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
import Ariel.Syntax.Transform

runNamed :: AST.Expr -> IO Expr
runNamed e = run emptyEnv emptyEnv (removeNames (makeLetRecs e))

run :: Env -> Env -> Expr -> IO Expr
run env renv (Pure e) = eval env renv e
run env renv (IOPrim p args) = runIOPrim p =<< traverse (eval env renv) args
run env renv (Bind (IOPrim p args) clos@Clos {}) = do
  t1 <- runIOPrim p =<< traverse (eval env renv) args
  run env renv =<< eval env renv (App clos t1)
run env renv (Bind (Pure t1) clos@Clos {}) = do
  t1' <- eval env renv t1
  run env renv =<< eval env renv (App clos t1')
run _ _ e@(Bind _ _) = error ("Invalid Bind: " <> show e)
run env renv e = run env renv =<< eval env renv e
