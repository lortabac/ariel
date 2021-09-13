{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Language.Types where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Data.Text (Text)
import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

data Module = Module Text [Decl]
  deriving (Eq, Show, Generic)

data Decl
  = Decl Text Expr
  | DeclLam Text [Text] Expr
  deriving (Eq, Show, Generic)

instance SexpIso Decl where
  sexpIso =
    match $
      With (list (el (sym "define") >>> el symbol >>> el sexpIso) >>>) $
        With
          (list (el (sym "define") >>> el symbol >>> el (list (rest symbol)) >>> el sexpIso) >>>)
          End

data Expr
  = Int Int
  | String Text
  | Con0 Text
  | Con Text [Expr]
  | Var Text
  | Lam1 Text Expr
  | Lam [Text] Expr
  | Case Expr [CaseEquation]
  | Let [LetDecl] Expr
  | Prim2 Prim2 Expr Expr
  | IOPrim (IOPrim Expr)
  | Bind Expr Expr
  | Pure Expr
  | App Expr [Expr]
  deriving (Eq, Show, Generic)

instance SexpIso Expr where
  sexpIso =
    match $
      With (int >>>) $
        With (string >>>) $
          With (quoted symbol >>>) $
            With (list (el (quoted symbol) >>> rest sexpIso) >>>) $
              With (symbol >>>) $
                With (list (el (sym "lambda") >>> el symbol >>> el sexpIso) >>>) $
                  With (list (el (sym "lambda") >>> el (list (rest symbol)) >>> el sexpIso) >>>) $
                    With (list (el (sym "case") >>> el sexpIso >>> rest sexpIso) >>>) $
                      With (list (el (sym "let") >>> el (list (rest sexpIso)) >>> el sexpIso) >>>) $
                        With (list (el (prefixed Comma (sym "prim2")) >>> el sexpIso >>> el sexpIso >>> el sexpIso) >>>) $
                          With (sexpIso >>>) $
                            With (list (el (sym ">>=") >>> el sexpIso >>> el sexpIso) >>>) $
                              With (list (el (sym "pure") >>> el sexpIso) >>>) $
                                With
                                  (list (el sexpIso >>> rest sexpIso) >>>)
                                  End

data Pattern
  = Pattern0 Text
  | Pattern Text [Text]
  deriving (Eq, Show, Generic)

instance SexpIso Pattern where
  sexpIso =
    match $
      With (quoted symbol >>>) $
        With
          (list (el (quoted symbol) >>> rest symbol) >>>)
          End

data CaseEquation = CaseEquation Pattern Expr
  deriving (Eq, Show, Generic)

instance SexpIso CaseEquation where
  sexpIso = with $ \eq ->
    list (el sexpIso >>> el sexpIso) >>> eq

data LetDecl = LetDecl Text Expr
  deriving (Eq, Show, Generic)

instance SexpIso LetDecl where
  sexpIso = with $ \decl ->
    list (el symbol >>> el sexpIso) >>> decl
