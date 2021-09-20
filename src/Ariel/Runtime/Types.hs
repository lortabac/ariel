module Ariel.Runtime.Types where

import Data.Text (Text)

data Value
  = VInt Int
  | VString Text
  | VBool Bool
  | VGlobal Int
  | VFun (Value -> Value)

instance Show Value where
  show (VInt i) = "VInt " ++ show i
  show (VString t) = "VString " ++ show t
  show (VBool b) = "VBool " ++ show b
  show (VGlobal i) = "VGlobal " ++ show i
  show (VFun _) = "VFun <>"

data PExpr
  = PConst Value
  | PVar Text
  | PAbs Text PExpr
  | PApp PExpr PExpr
  deriving (Show)

data MExpr
  = MConst Value
  | MApp MExpr MExpr
  deriving (Show)
