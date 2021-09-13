module Ariel.Runtime.IO where

import Ariel.Runtime.Eval
import Ariel.Runtime.IOPrim
import Ariel.Runtime.Types
import Data.Vector (Vector)

run :: Vector Expr -> Expr -> IO Expr
run globals (Pure e) = eval globals e
run _ (IOPrim p) = runIOPrim p
run globals (Bind expr k) = do
  expr' <- eval globals expr
  case expr' of
    IOPrim p -> do
      r <- runIOPrim p
      run globals =<< eval globals (App1 k r)
    Pure e -> run globals (App1 k e)
    _ -> error "Invalid bind"
run globals e = run globals =<< eval globals e
