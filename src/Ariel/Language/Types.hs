{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Language.Types where

import Ariel.Common.Types
import Control.Lens
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

data Module = Module Text [Name] [ImportDecl] [Decl]
  deriving (Eq, Show, Generic)

instance SexpIso Module where
  sexpIso = with $
    \m -> list (el (sym "module") >>> el symbol >>> el sexpIso >>> el sexpIso >>> rest sexpIso) >>> m

newtype ImportDecl = ImportDecl Name
  deriving (Eq, Show, Generic)

instance SexpIso ImportDecl where
  sexpIso = with $
    \iDecl -> list (el (sym "import") >>> el sexpIso) >>> iDecl

data Decl
  = Decl Name Expr
  | DeclLam (NonEmpty Name) Expr
  | TypeDecl Name [ConDecl]
  deriving (Eq, Show, Generic)

instance SexpIso Decl where
  sexpIso =
    match $
      With (list (el (sym "define") >>> el sexpIso >>> el sexpIso) >>>) $
        With (list (el (sym "define") >>> el sexpIso >>> el sexpIso) >>>) $
          With (list (el (sym "define-type") >>> el sexpIso >>> rest sexpIso) >>>) End

data ConDecl = ConDecl Tag [Ty]
  deriving (Eq, Show, Generic)

instance SexpIso ConDecl where
  sexpIso = with $
    \cd -> bracketList (el sexpIso >>> rest sexpIso) >>> cd

data Expr
  = Int Int
  | String Text
  | Con Tag
  | Lam [Name] Expr
  | Let [LetDecl] Expr
  | Prim Text
  | Case Expr [CaseEquation]
  | Var Name
  | App Expr [Expr]
  deriving (Eq, Show, Generic)

instance SexpIso Expr where
  sexpIso =
    match $
      With (int >>>) $
        With (string >>>) $
          With (sexpIso >>>) $
            With (list (el (sym "lambda") >>> el sexpIso >>> el sexpIso) >>>) $
              With (list (el (sym "let") >>> el (list (rest sexpIso)) >>> el sexpIso) >>>) $
                With (prefixed Comma symbol >>>) $
                  With (list (el (sym "match") >>> el sexpIso >>> el (list (rest sexpIso))) >>>) $
                    With (sexpIso >>>) $
                      With (list (el sexpIso >>> rest sexpIso) >>>) End

data LetDecl = LetDecl Name Expr
  deriving (Eq, Show, Generic)

instance SexpIso LetDecl where
  sexpIso = with $
    \letDecl -> bracketList (el sexpIso >>> el sexpIso) >>> letDecl

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

buildDefs :: [Module] -> Defs
buildDefs = mconcat . map moduleDefs
  where
    moduleDefs (Module name _ _ decls) = foldr (insertDef name) mempty decls
    insertDef modName (Decl n@(Name name) _) defs =
      defs
        & #terms %~ insertAppend n (QName modName name)
    insertDef modName (DeclLam (n@(Name name) :| _) _) defs =
      defs
        & #terms %~ insertAppend n (QName modName name)
    insertDef modName (TypeDecl name decls) defs =
      foldr
        (insertConDef modName)
        (insertTypeDef modName name defs)
        decls
    insertTypeDef modName n@(Name name) defs =
      defs
        & #types %~ insertAppend n (QName modName name, Sum)
    insertConDef modName (ConDecl (Tag name) _) defs =
      defs
        & #terms %~ insertAppend (Name name) (QName modName name)

insertAppend :: Ord k => k -> a -> Map k [a] -> Map k [a]
insertAppend k v = Map.insertWith (++) k [v]
