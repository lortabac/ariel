module Ariel.Language.Desugar where

import Ariel.Common.Types
import qualified Ariel.Core.Types as Core
import Ariel.Language.Types
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map

desugarDecl :: Decl -> Core.Decl
desugarDecl (Decl (Name name) e) = Core.Decl name (desugarExpr e)
desugarDecl (DeclLam (Name name :| args) e) = Core.Decl name (desugarExpr (Lam args e))
desugarDecl (TypeDecl (Name name) decls) = Core.TypeDecl name $
  Map.fromList (map (\(ConDecl tag ts) -> (tag, ts)) decls)

desugarExpr :: Expr -> Core.Expr
desugarExpr (Int i) = Core.Int i
desugarExpr (String s) = Core.String s
desugarExpr (Con _) = error "Unimplemented"
desugarExpr (Lam names e) = foldr (\(Name name) -> Core.Lam name) (desugarExpr e) names
desugarExpr (Let decls e) = foldr
  (\(LetDecl (Name name) e1) -> Core.Let name (desugarExpr e1)) (desugarExpr e) decls
desugarExpr (Prim name) = Core.Prim name
desugarExpr (Case _ _) = error "Unimplemented"
desugarExpr (Var (Name x)) = Core.Var x -- TODO Implement globals
desugarExpr (App f es) = foldl' Core.App (desugarExpr f) (map desugarExpr es)
