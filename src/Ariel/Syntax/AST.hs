{-# LANGUAGE StrictData #-}

module Ariel.Syntax.AST where

import Ariel.Syntax.Ty
import Ariel.Syntax.Types
import Data.List (foldl')
import Data.Map (Map)
import Data.Text (Text)
import Data.Functor.Fixedpoint (Fix)

-- | Ariel expression
data Expr
  = Int Int
  | Double Double
  | Text Text
  | Cons Tag [Expr]
  | QCons QTag [Expr]
  | Record Name (Map Label Expr)
  | QRecord QName (Map Label Expr)
  | Get Label Expr
  | QGet QLabel Expr
  | Lam Name Expr
  | MultiLam [Name] Expr
  | App Expr Expr
  | MultiApp Expr [Expr]
  | Var Name
  | QVar QName
  | Case Expr (Map Tag Expr)
  | Let Name Expr Expr
  | MultiLet [TermDecl] Expr
  | LetRec Name Expr Expr
  | MultiLetRec [TermDecl] Expr
  | Bind Expr Expr
  | Pure Expr
  | Annot Expr (Fix Ty)
  deriving (Eq, Show)

data TermDecl
  = TermDecl Name Expr
  deriving (Eq, Show)

data Assoc = InfixL | InfixR | InfixN
  deriving (Eq)

data OperatorInfo = OperatorInfo
  { assoc :: Assoc,
    prec :: Int
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
