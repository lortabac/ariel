{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Core.Types where

import Ariel.Common.Types
import Control.DeepSeq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics
import Language.SexpGrammar
import Language.SexpGrammar.Generic

data Module = Module Text [Decl]
  deriving (Eq, Show, Generic)

instance SexpIso Module where
  sexpIso = with $
    \m -> list (el (sym "module") >>> el symbol >>> rest sexpIso) >>> m

data Decl
  = Decl Text Expr
  | TypeDecl Text (Map Tag [Ty])
  deriving (Eq, Show, Generic)

instance SexpIso Decl where
  sexpIso = match $
    With (list (el (sym "define") >>> el symbol >>> el sexpIso) >>>) $
      With (list (el (sym "define-type") >>> el symbol >>> el sexpIso) >>>) End

data Expr
  = Int Int
  | String Text
  | Con QName Tag
  | Global QName
  | Lam Text Expr
  | Let Text Expr Expr
  | Prim Text
  | Case Expr (Map Tag Expr)
  | Var Text
  | App Expr Expr
  deriving (Eq, Show, Generic)

instance NFData Expr

instance SexpIso Expr where
  sexpIso = match $
    With (int >>>) $
      With (string >>>) $
        With (list (el sexpIso >>> el sexpIso) >>>) $
          With (sexpIso >>>) $
            With (list (el (sym "lambda") >>> el symbol >>> el sexpIso) >>>) $
              With (list (el (sym "let") >>> el (list (el symbol >>> el sexpIso)) >>> el sexpIso) >>>) $
                With (prefixed Comma symbol >>>) $
                  With (list (el (sym "case") >>> el sexpIso >>> el sexpIso) >>>) $
                    With (symbol >>>) $
                      With (list (el sexpIso >>> el sexpIso) >>>) End

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
    sumTypes :: Map QName (Map Tag [Ty])
  }
  deriving (Eq, Show)

instance Semigroup Defs where
  Defs gs1 st1 <> Defs gs2 st2 = Defs (gs1 <> gs2) (st1 <> st2)

instance Monoid Defs where
  mempty = Defs mempty mempty

buildDefs :: [Module] -> Defs
buildDefs = mconcat . map moduleDefs
  where
    moduleDefs (Module name decls) = foldr (insertDef name) mempty decls
    insertDef modName (Decl name e) defs = defs { globals = Map.insert (QName modName name) e (globals defs) }
    insertDef modName (TypeDecl name ts) defs = defs { sumTypes = Map.insert (QName modName name) ts (sumTypes defs) }
