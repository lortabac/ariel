module Ariel.Core.Types where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Ariel.Common.Types
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

data Expr = Int Int
          | String Text
          | Con QName Tag [Expr]
          | Var Text
          | Global QName
          | Lam [Text] Expr
          | Fix Text Expr
          | App Expr [Expr]
          | Case Expr (Map Tag Expr)
          | Let Text Expr Expr
          | Prim2 Prim2 Expr Expr
          | IOPrim (IOPrim Expr)
          | Bind Expr Expr
          | Pure Expr
          deriving (Eq, Show)

-- | Convenience operator for lambdas
(==>) :: [Text] -> Expr -> Expr
(==>) = Lam

infixr 1 ==>

-- | Convenience operator for applications
(@@) :: Expr -> [Expr] -> Expr
(@@) = App

infixl 9 @@

data Defs = Defs
    { globals :: Map QName Expr
    , sumTypes :: Map QName (Set Tag)
    } deriving (Eq, Show)

instance Semigroup Defs where
    Defs gs1 st1 <> Defs gs2 st2 = Defs (gs1 <> gs2) (st1 <> st2)

instance Monoid Defs where
    mempty = Defs mempty mempty
