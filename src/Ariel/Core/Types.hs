{-# LANGUAGE DeriveGeneric #-}

module Ariel.Core.Types where

import Ariel.Common.IOPrim
import Ariel.Common.Types
import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics

data Expr
  = Int Int
  | String Text
  | Con QName Tag
  | Var Text
  | Global QName
  | Lam Text Expr
  | Fix Text Expr
  | App Expr Expr
  | Case Expr (Map Tag Expr)
  | Let Text Expr Expr
  | Prim Text
  | IOPrim (IOPrim Expr)
  | Bind Expr Expr
  | Pure Expr
  deriving (Eq, Show, Generic)

instance NFData Expr

-- | Convenience operator for lambdas
(==>) :: Text -> Expr -> Expr
(==>) = Lam

infixr 1 ==>

-- | Convenience operator for applications
(@@) :: Expr -> Expr -> Expr
(@@) = App

infixl 9 @@

data Defs = Defs
  { globals :: Map QName Expr,
    sumTypes :: Map QName (Map Tag Int)
  }
  deriving (Eq, Show)

instance Semigroup Defs where
  Defs gs1 st1 <> Defs gs2 st2 = Defs (gs1 <> gs2) (st1 <> st2)

instance Monoid Defs where
  mempty = Defs mempty mempty
