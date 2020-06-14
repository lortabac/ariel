{-# LANGUAGE StrictData #-}

module Ariel.Syntax.AST where

import Ariel.Syntax.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.List (foldl')

-- | Ariel expression
data Expr
  = Int Int
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
  | Prim1 Name Expr
  | Prim2 Name Expr Expr
  | IOPrim Name [Expr]
  | Bind Expr Expr
  | Pure Expr
  deriving (Eq, Show)

data TermDecl
  = TermDecl Name Expr



data Assoc = InfixL | InfixR | InfixN
           deriving(Eq)

data OperatorInfo = OperatorInfo
                  { assoc :: Assoc
                  , prec  :: Int
                  }

data Decl
  = TermBinding TermDecl
  | OperatorDecl Name OperatorInfo

data ReplStmt
  = Expr Expr
  | Decl Decl


-- | Convenience function for creating curried lambdas
--   Note that a lambda with 0 args is simply an expr
variadicLambda :: [Name] -> Expr -> Expr
variadicLambda xs e = foldr Lam e xs

-- | Convenience function for applying a function to
--   multiple arguments, left associatively
variadicApply :: Expr -> [Expr] -> Expr
variadicApply f args = foldl' App f args

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

-- | Convenience operator for let rec expressions
inrec :: (Name, Expr) -> Expr -> Expr
inrec (n, e1) e2 = LetRec n e1 e2

infixr 2 `inrec`

-- | Convenience operator for bind
(>>==) :: Expr -> Expr -> Expr
(>>==) = Bind

infixl 1 >>==
