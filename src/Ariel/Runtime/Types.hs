{-# LANGUAGE OverloadedStrings #-}

module Ariel.Runtime.Types where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Ariel.Common.Types
import Data.Text (Text)
import Data.Vector (Vector)

data Expr
  = Int Int
  | String Text
  | Con Tag Int [Expr]
  | Global Int
  | Lam1 (Expr -> Expr)
  | Lam2 (Expr -> Expr -> Expr)
  | Lam3 (Expr -> Expr -> Expr -> Expr)
  | Fix (Expr -> Expr)
  | App1 Expr Expr
  | App2 Expr Expr Expr
  | App3 Expr Expr Expr Expr
  | Case Expr (Vector Expr)
  | Prim2 Prim2 Expr Expr
  | IOPrim (IOPrim Expr)
  | Bind Expr Expr
  | Pure Expr

unit :: Expr
unit = Con "unit" 0 []

false :: Expr
false = Con "false" 0 []

true :: Expr
true = Con "true" 1 []
