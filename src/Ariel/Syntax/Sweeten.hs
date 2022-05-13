{-# LANGUAGE OverloadedStrings #-}

module Ariel.Syntax.Sweeten where

import Ariel.Syntax.Types
import Data.List.NonEmpty (NonEmpty (..))

sweetenExpr :: Expr -> Expr
sweetenExpr (Lam vars1 (Lam vars2 e)) = sweetenExpr $ Lam (vars1 ++ vars2) (sweetenExpr e)
sweetenExpr (Lam vars e) = Lam vars (sweetenExpr e)
sweetenExpr (Let decls1 (Let decls2 e)) = sweetenExpr $ Let (decls1 ++ decls2) (sweetenExpr e)
sweetenExpr (Let decls e) = Let decls (sweetenExpr e)
sweetenExpr (App (App (e :| es1) :| es2)) = sweetenExpr $ App (e :| es1 <> es2)
sweetenExpr (App es) = App (sweetenExpr <$> es)
sweetenExpr e = e

sweetenTy :: Ty -> Ty
sweetenTy (TArr (t : TArr ts1 : ts2)) = sweetenTy $ TArr (t : ts1 ++ ts2)
sweetenTy (TArr ts) = TArr (sweetenTy <$> ts)
sweetenTy (TApp (TApp (t :| ts1) :| ts2)) = sweetenTy $ TApp (t :| ts1 <> ts2)
sweetenTy (TApp (TSym "->" :| ts)) = sweetenTy $ TArr (sweetenTy <$> ts)
sweetenTy (TApp ts) = TApp (sweetenTy <$> ts)
sweetenTy (Forall vars t) = Forall vars (sweetenTy t)
sweetenTy t = t
