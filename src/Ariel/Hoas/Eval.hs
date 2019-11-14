{-# LANGUAGE DataKinds #-}

module Ariel.Hoas.Eval where

import Ariel.Common.Env
import Ariel.Common.Types
import Ariel.Hoas.Expr
import Ariel.Hoas.IO
import Ariel.Hoas.Value
import Ariel.Language.Expr
import Data.Maybe (fromMaybe)
import Data.Vector ((!?), Vector)
import Data.Void (absurd)

run :: Env ExprH -> ExprH -> IO ExprH
run env expr = case expr of
  IOH f -> f
  BindH (IOH f) (LamH g) -> f >>= run env . g eval1
  e -> run env (eval1 env e)

runCore :: Env ExprH -> Expr 'Core -> IO ExprH
runCore env = run env . toExprH env

eval :: Env ExprH -> ExprH -> ExprH
eval env e = if isVal e1 then e1 else eval env e1
  where
    e1 = eval1 env e

evalCore :: Env ExprH -> Expr 'Core -> ExprH
evalCore env = eval env . toExprH env

eval1 :: Env ExprH -> ExprH -> ExprH
eval1 env expr = case expr of
  IntH i -> IntH i
  FloatH f -> FloatH f
  StringH s -> StringH s
  ConsH ix e -> ConsH ix (eval1 env e)
  TupleH xs -> TupleH (eval1 env <$> xs)
  AtH (TupleH xs) (TupleIx ix) -> fromMaybe (error "Tuple index out of range") (xs !? (ix - 1))
  AtH _ _ -> error "Cannot access a non-tuple by index"
  LamH f -> LamH f
  AppH (VarH name) x -> AppH (eval1 env (VarH name)) x
  AppH (AppH f x) y -> AppH (eval1 env (AppH f x)) y
  AppH (LamH f) x -> f eval1 x
  AppH _ _ -> error "Cannot apply a non-function"
  VarH name -> case lookupEnv name env of
    Just x -> x
    Nothing -> error ("Unknown definition: " ++ unName name)
  CaseH e xs -> evalCase env e xs
  LetH f -> f eval1
  FixH f -> f eval1
  IOH io -> IOH io
  BindH f g -> BindH (eval1 env f) g
  PureH e -> IOH $ pure (eval1 env e)

evalCase :: Env ExprH -> ExprH -> Vector ExprH -> ExprH
evalCase env expr eqs = case eval env expr of
  ConsH (ConsIx ix) e -> eq ix `AppH` e
  _ -> error "Case on a non-constructor"
  where
    eq i = fromMaybe (error "Constructor index out of range") (eqs !? i)

mapToExprH :: Env ExprH -> Env (Expr 'Core) -> Env ExprH
mapToExprH env = fmap (toExprH env)

toExprH :: Env ExprH -> Expr 'Core -> ExprH
toExprH env expr = case expr of
  Int i -> IntH i
  Float f -> FloatH f
  String s -> StringH s
  ConsC (Cons ix e) -> ConsH ix (toExprH env e)
  VariantC v -> absurd v
  Tuple xs -> TupleH (toExprH env <$> xs)
  At xs ix -> AtH (toExprH env xs) ix
  Lam param body -> makeLamH env param body
  App f x -> AppH (toExprH env f) (toExprH env x)
  Var name -> VarH name
  Case e xs -> CaseH (toExprH env e) (toExprH env <$> xs)
  Let e1 e2 body -> makeLetH env e1 e2 body
  Fix rec e -> makeFixH env rec e
  IOPrim name -> ioPrim env name
  Bind f g -> BindH (toExprH env f) (toExprH env g)
  Pure e -> PureH (toExprH env e)

makeLamH :: Env ExprH -> Expr 'Core -> Expr 'Core -> ExprH
makeLamH env (Var name) body = LamH $ \k x ->
  let newEnv = insertEnv name x env
   in k newEnv (toExprH newEnv body)
makeLamH _ _ _ = error "Invalid function"

makeLetH :: Env ExprH -> Expr 'Core -> Expr 'Core -> Expr 'Core -> ExprH
makeLetH env (Var name) expr body = LetH $ \k ->
  let newEnv = insertEnv name hexpr env
      hexpr = toExprH env expr
      hbody = toExprH newEnv body
   in k newEnv hbody
makeLetH _ _ _ _ = error "Invalid let expression"

makeFixH :: Env ExprH -> Expr 'Core -> Expr 'Core -> ExprH
makeFixH env (Var name) expr = FixH $ \k ->
  let newEnv = insertEnv name rec env
      hexpr = toExprH newEnv expr
      rec = FixH $ \f -> f newEnv hexpr
   in k newEnv hexpr
makeFixH _ _ _ = error "Invalid fix expression"
