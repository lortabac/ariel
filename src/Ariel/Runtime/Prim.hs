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
primMap =
  Map.fromList
    [ ("fix", recur),
      ("i=", intEq),
      ("i<", intLT),
      ("i+", intPlus),
      ("i-", intMinus)
    ]

recur :: PExpr
recur = PConst $ VFun $ \(VFun f) -> fix f

intEq :: PExpr
intEq = PConst $ makeFun2 $ \(VInt x) (VInt y) -> if x == y then true else false

intLT :: PExpr
intLT = PConst $ makeFun2 $ \(VInt x) (VInt y) -> if x < y then true else false

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

false :: Value
false = makeFun2 const

true :: Value
true = makeFun2 (\_ x -> x)
