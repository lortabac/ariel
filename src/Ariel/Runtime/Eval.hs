{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Ariel.Runtime.Eval where

import Ariel.Common.Types
import Ariel.Runtime.Env
import Ariel.Runtime.IOPrim
import Ariel.Runtime.Nameless
import Ariel.Runtime.Prim
import Ariel.Runtime.Types
import Data.Vector (Vector, (!))

run :: Vector IExpr -> Env Value -> IExpr -> IO Value
run globals env e = case eval globals env e of
  VIOPrim p -> run globals env . IConst =<< runIOPrim p
  VBindIO v1 v2 -> case (v1, v2) of
    (VIOPrim p, clos@VClos {}) -> do
      io <- runIOPrim p
      run globals env (IApp (IConst clos) (IConst io))
    _ -> error ("Invalid bind arguments: " <> show (v1, v2))
  v -> pure v

eval :: Vector IExpr -> Env Value -> IExpr -> Value
eval _ _ (IConst v) = v
eval globals env (IGlobal i) = eval globals env (globals ! i)
eval _ env (IVar name i) = case lookupEnv i env of
  Just e -> e
  Nothing -> error ("Out of scope variable: " <> showName name)
eval _ env (IAbs name e) = VClos env name e
eval _ env (IDummyAbs name e) = VDummyClos env name e
eval globals env (IApp e1 e2) = case (# eval globals env e1, eval globals env e2 #) of
  (# VClos closEnv _ body, v2 #) -> eval globals (extendEnv v2 closEnv) body
  (# VDummyClos closEnv _ body, _ #) -> eval globals closEnv body
  (# v1, _ #) -> error ("Invalid app term: " <> show v1)
eval globals env (IPrim p) = evalPrim (fmap (eval globals env) p)
eval globals env (IIOPrim p) = VIOPrim (fmap (eval globals env) p)
eval globals env (IIf e t f) = case eval globals env e of
  (VBool b) -> case b of
    0# -> eval globals env f
    _ -> eval globals env t
  v -> error ("Invalid if condition: " <> show v)
eval globals env (IBindIO e1 e2) = VBindIO (eval globals env e1) (eval globals env e2)
eval globals env (IFix e) = case eval globals env e of
  VClos closEnv _ body ->
    let res = eval globals (extendEnv res closEnv) body
     in res
  v -> error ("Invalid fix: " <> show v)

evalPExpr :: Vector IExpr -> PExpr -> Value
evalPExpr globals = eval globals mempty . removeNames
