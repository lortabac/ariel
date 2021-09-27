{-# LANGUAGE StrictData #-}

module Ariel.Runtime.Types where

import Data.Text (Text)

data Value
  = VInt Int
  | VString Text
  | VGlobal Int
  | VFun (Value -> Value)
  | VAction (IO Value)

instance Show Value where
  show (VInt i) = "VInt " ++ show i
  show (VString t) = "VString " ++ show t
  show (VGlobal i) = "VGlobal " ++ show i
  show (VFun _) = "VFun"
  show (VAction _) = "VAction"

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
