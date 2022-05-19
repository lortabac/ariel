{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Ariel.Core.Types where

import Ariel.Common.Types
import Ariel.Prelude
import Control.DeepSeq
import Control.Lens (Plated, children, transformM)
import GHC.Generics
import Language.Sexp.Located (Position, dummyPos)
import Logic.Unify

data Decl
  = Decl Text Expr
  | TypeDecl Text (Map Tag [Ty])
  deriving (Eq, Show, Generic)

data Ty
  = TCon Name
  | TApp Ty Ty
  | TVar TyVar
  | Forall [TyVar] Ty
  | Metavar UVar
  deriving (Eq, Show, Ord, Generic, Data)

pattern TArr :: Ty -> Ty -> Ty
pattern TArr t1 t2 = TApp (TApp (TCon "->") t1) t2

pattern TInt :: Ty
pattern TInt = TCon "Int"

pattern TString :: Ty
pattern TString = TCon "String"

pattern TBool :: Ty
pattern TBool = TCon "Bool"

instance Plated Ty

instance Unifiable Ty where
  getVar (Metavar v) = Just v
  getVar _ = Nothing

  transformTermM = transformM

  termChildren = children

data Expr
  = Int Int
  | String Text
  | Bool Bool
  | Con QName Tag
  | Global QName
  | Lam Position Name Expr
  | Let Position Name Expr Expr
  | Prim Position Text [Expr]
  | IOPrim Position Text [Expr]
  | Case Expr (Map Tag Expr)
  | Var Position Name
  | App Position Expr Expr
  | If Position Expr Expr Expr
  | BindIO Expr Expr
  | Fix Position Expr
  deriving (Eq, Show, Generic)

instance NFData Expr

-- | Convenience constructor for vars
var :: Name -> Expr
var = Var dummyPos

-- | Convenience operator for lambdas
(==>) :: Name -> Expr -> Expr
(==>) = Lam dummyPos

infixr 1 ==>

-- | Convenience operator for applications
(@@) :: Expr -> Expr -> Expr
(@@) = App dummyPos

infixl 9 @@

data Defs = Defs
  { globals :: [(QName, Expr)],
    sumTypes :: Map QName (Map Tag [Ty])
  }
  deriving (Eq, Show)

instance Semigroup Defs where
  Defs gs1 st1 <> Defs gs2 st2 = Defs (gs1 <> gs2) (st1 <> st2)

instance Monoid Defs where
  mempty = Defs mempty mempty
