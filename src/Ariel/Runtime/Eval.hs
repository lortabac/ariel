module Ariel.Runtime.Eval where

import Ariel.Runtime.Compile
import Ariel.Runtime.Types
import Data.Vector (Vector, (!))

eval :: Vector MExpr -> MExpr -> Value
eval globals (MConst v) = evalMConst globals v
eval globals (MApp e1 e2) = case (eval globals e1, eval globals e2) of
  (VFun f, v) -> f v
  _ -> error "Eval error"

evalMConst :: Vector MExpr -> Value -> Value
evalMConst globals (VGlobal i) = eval globals (globals ! i)
evalMConst _ v = v

evalPExpr :: Vector MExpr -> PExpr -> Value
evalPExpr globals = eval globals . compile
