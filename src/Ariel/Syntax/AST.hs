module Ariel.Syntax.AST where

import Ariel.Syntax.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

-- | Ariel expression
data Expr
  = Int Integer
  | Double Double
  | Text Text
  | CoreCons ConsIx [Expr]
  | Cons Tag [Expr]
  | Tuple (Vector Expr)
  | GetT TupleIx Expr
  | UpdateT TupleIx Expr Expr
  | Lam Name Expr
  | App Expr Expr
  | Var Name
  | RecVar Name
  | CoreCase Expr (Vector Expr)
  | Case Expr (Map Tag Expr)
  | Let Name Expr Expr
  | LetRec Name Expr Expr
  | Prim Name [Expr]
  | IOPrim Name [Expr]
  | Bind Expr Expr
  | Pure Expr
  deriving (Eq, Show)

-- | Convenience operator for lambdas
(==>) :: Name -> Expr -> Expr
(==>) = Lam

infixr 1 ==>

-- | Convenience operator for applications
(@@) :: Expr -> Expr -> Expr
(@@) = App

infixl 9 @@

-- | Convenience operator for let expressions
in_ :: (Name, Expr) -> Expr -> Expr
in_ (n, e1) e2 = Let n e1 e2

infixr 2 `in_`

-- | Convenience operator for bind
(>>==) :: Expr -> Expr -> Expr
(>>==) = Bind

infixl 1 >>==
