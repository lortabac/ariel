{-# LANGUAGE OverloadedStrings #-}

module Ariel.Syntax.Sweeten where

import Ariel.Syntax.Types
import Data.List.NonEmpty (NonEmpty (..))

sweetenExpr :: Expr -> Expr
sweetenExpr (Lam p vars1 (Lam _ vars2 e)) = sweetenExpr $ Lam p (vars1 ++ vars2) (sweetenExpr e)
sweetenExpr (Lam p vars e) = Lam p vars (sweetenExpr e)
sweetenExpr (Let p decls1 (Let _ decls2 e)) = sweetenExpr $ Let p (decls1 ++ decls2) (sweetenExpr e)
sweetenExpr (Let p decls e) = Let p decls (sweetenExpr e)
sweetenExpr (App p (App _ (e :| es1) :| es2)) = sweetenExpr $ App p (e :| es1 <> es2)
sweetenExpr (App p es) = App p (sweetenExpr <$> es)
sweetenExpr e = e

sweetenTy :: Ty -> Ty
sweetenTy (TArr (t : TArr ts1 : ts2)) = sweetenTy $ TArr (t : ts1 ++ ts2)
sweetenTy (TArr ts) = TArr (sweetenTy <$> ts)
sweetenTy (TApp (TApp (t :| ts1) :| ts2)) = sweetenTy $ TApp (t :| ts1 <> ts2)
sweetenTy (TApp (TSym "->" :| ts)) = sweetenTy $ TArr (sweetenTy <$> ts)
sweetenTy (TApp ts) = TApp (sweetenTy <$> ts)
sweetenTy (Forall vars t) = Forall vars (sweetenTy t)
sweetenTy t = t
