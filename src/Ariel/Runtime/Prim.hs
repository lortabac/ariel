{-# LANGUAGE OverloadedStrings #-}
module Ariel.Runtime.Prim where

import Ariel.Runtime.Types
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

lookupPrim :: Text -> PExpr
lookupPrim name = case Map.lookup name primMap of
  Just e -> e
  Nothing -> error "Invalid prim"

primMap :: Map Text PExpr
primMap = Map.fromList
  [ ("fix", recur)
  , ("if", condition)
  , ("i=", intEq)
  , ("i+", intPlus)
  , ("i-", intMinus)
  ]

-- match :: PExpr
-- match = PConst $ makeFun2 $ \(VVec v) (VVec eqs) -> apply v eqs
--   where
--     apply v eqs = eq (VVec args)
--       where
--         index = case V.head v of
--           VInt i -> i
--           _ -> error "Invalid constructor"
--         args = V.tail v
--         eq = case eqs ! index of
--           VFun f -> f
--           _ -> error "Invalid case equation"

condition :: PExpr
condition = PConst $ makeFun3 $ \(VBool b) e1 e2 ->
  if b then e2 else e1

recur :: PExpr
recur = PConst $ VFun $ \(VFun f) -> fix f

intEq :: PExpr
intEq = PConst $ makeFun2 $ \(VInt x) (VInt y) -> VBool (x == y)

intPlus :: PExpr
intPlus = PConst $ makeFun2 $ \(VInt x) (VInt y) -> VInt (x + y)

intMinus :: PExpr
intMinus = PConst $ makeFun2 $ \(VInt x) (VInt y) -> VInt (x - y)

plus3 :: PExpr
plus3 = PConst $ makeFun3 $ \(VInt x) (VInt y) (VInt z) -> VInt (x + y + z)

makeFun :: (Value -> Value) -> Value
makeFun = VFun

makeFun2 :: (Value -> Value -> Value) -> Value
makeFun2 f = VFun (VFun . f)

makeFun3 :: (Value -> Value -> Value -> Value) -> Value
makeFun3 f = VFun (\x -> VFun (VFun . f x))

-- con :: Int -> [Value] -> Value
-- con i es = VFun $ \i -> VVec $ V.fromList list
--   where
--     list = VInt i : es
