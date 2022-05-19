{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Syntax.Types where

import Ariel.Common.Types
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

newtype ImportDecl = ImportDecl Name
  deriving (Eq, Show, Generic)

instance SexpIso ImportDecl where
  sexpIso = with $
    \iDecl -> list (el (sym "import") >>> el sexpIso) >>> iDecl

data Decl
  = Decl Position Name Expr
  | DeclLam Position (NonEmpty Name) Expr
  deriving (Eq, Show, Generic)

instance SexpIso Decl where
  sexpIso =
    match $
      With (\d -> position >>> swap >>> list (el (sym "define") >>> el sexpIso >>> el sexpIso) >>> d) $
        With
          (\d -> position >>> swap >>> list (el (sym "define") >>> el sexpIso >>> el sexpIso) >>> d)
          End

data Expr
  = Int Int
  | String Text
  | F
  | T
  | Con Tag
  | Lam Position [Name] Expr
  | Let Position [LetDecl] Expr
  | Prim Position Text [Expr]
  | IOPrim Position Text [Expr]
  | Case Expr [CaseEquation]
  | If Position Expr Expr Expr
  | Fix Position Expr
  | NamedLam Position Name [Name] Expr
  | BindIO Expr Expr
  | Var Position Name
  | App Position (NonEmpty Expr)
  deriving (Eq, Show, Generic)

instance SexpIso Expr where
  sexpIso =
    match $
      With (int >>>) $
        With (string >>>) $
          With (hashed (sym "f") >>>) $
            With (hashed (sym "t") >>>) $
              With (sexpIso >>>) $
                With (\l -> position >>> swap >>> list (el (sym "lambda") >>> el sexpIso >>> el sexpIso) >>> l) $
                  With (\l -> position >>> swap >>> list (el (sym "let") >>> el (list (rest sexpIso)) >>> el sexpIso) >>> l) $
                    With (\p -> position >>> swap >>> list (el (hashed (sym "prim")) >>> el symbol >>> rest sexpIso) >>> p) $
                      With (\p -> position >>> swap >>> list (el (sym "io-prim") >>> el symbol >>> rest sexpIso) >>> p) $
                        With (list (el (sym "match") >>> el sexpIso >>> el (list (rest sexpIso))) >>>) $
                          With (\i -> position >>> swap >>> list (el (sym "if") >>> el sexpIso >>> el sexpIso >>> el sexpIso) >>> i) $
                            With (\f -> position >>> swap >>> list (el (sym "fix") >>> el sexpIso) >>> f) $
                              With (\n -> position >>> swap >>> list (el (sym "named-lambda") >>> el sexpIso >>> el sexpIso >>> el sexpIso) >>> n) $
                                With (list (el (sym "bind-io") >>> el sexpIso >>> el sexpIso) >>>) $
                                  With (\v -> position >>> swap >>> sexpIso >>> v) $
                                    With
                                      (\app -> position >>> swap >>> sexpIso >>> app)
                                      End

data LetDecl = LetDecl Name Expr
  deriving (Eq, Show, Generic)

instance SexpIso LetDecl where
  sexpIso = with $
    \letDecl -> bracketList (el sexpIso >>> el sexpIso) >>> letDecl

letDeclFromPair :: (Name, Expr) -> LetDecl
letDeclFromPair (name, expr) = LetDecl name expr

data Pat
  = Pat0 Tag
  | Pat Tag [Name]
  deriving (Eq, Show, Generic)

instance SexpIso Pat where
  sexpIso =
    match $
      With (sexpIso >>>) $
        With (list (el sexpIso >>> rest sexpIso) >>>) End

data CaseEquation = CaseEquation Pat Expr
  deriving (Eq, Show, Generic)

instance SexpIso CaseEquation where
  sexpIso = with $
    \eq -> bracketList (el sexpIso >>> el sexpIso) >>> eq

data Defs = Defs
  { terms :: Map Name [QName],
    types :: Map Name [(QName, TyDef)]
  }
  deriving (Eq, Show, Generic)

instance Semigroup Defs where
  Defs tms1 tys1 <> Defs tsm2 tys2 = Defs (tms1 <> tsm2) (tys1 <> tys2)

instance Monoid Defs where
  mempty = Defs mempty mempty

data TyDef = Sum | Record
  deriving (Eq, Show, Generic)

data Ty
  = Forall [TyVar] Ty
  | TArr [Ty]
  | TSym Name
  | TApp (NonEmpty Ty)
  deriving (Eq, Show, Generic)

instance SexpIso Ty where
  sexpIso =
    match $
      With (list (el (sym "forall") >>> el sexpIso >>> el sexpIso) >>>) $
        With (list (el (sym "->") >>> rest sexpIso) >>>) $
          With (sexpIso >>>) $
            With
              (sexpIso >>>)
              End
