module Ariel.Language.Desugar where

import Ariel.Language.Types

desugarDecl :: Decl -> Decl
desugarDecl d@Decl {} = d
desugarDecl (DeclLam name args e) = Decl name (desugarExpr (Lam args e))

desugarExpr :: Expr -> Expr
desugarExpr i@Int {} = i
