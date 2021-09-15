{-# LANGUAGE MagicHash #-}

module Ariel.Evaluation.Nameless
  ( removeNames,
  )
where

import Ariel.Evaluation.Types
import qualified Ariel.Syntax.AST as AST
import Ariel.Syntax.Types
import Data.List (elemIndex)
import GHC.Exts (Int (..))

-- | Convert a fully-desugared AST expression to a nameless core expression
removeNames :: AST.Expr -> Expr
removeNames = removeNames' [] []

removeNames' :: [Name] -> [Name] -> AST.Expr -> Expr
removeNames' ctx _ (AST.Var x) = case elemIndex x ctx of
  Just i -> Var x (VarIx i)
  Nothing -> error ("Free variable " <> unName x)
removeNames' _ recCtx (AST.RecVar x) = case elemIndex x recCtx of
  Just i -> RecVar x (VarIx i)
  Nothing -> error ("Free recursive variable " <> unName x)
removeNames' ctx recCtx (AST.Lam x e) = Abs x (removeNames' (x : ctx) recCtx e)
removeNames' ctx recCtx (AST.App e1 e2) = App (removeNames' ctx recCtx e1) (removeNames' ctx recCtx e2)
removeNames' _ _ (AST.Int (I# x)) = Int x
removeNames' _ _ (AST.Double x) = Double x
removeNames' _ _ (AST.Text x) = Text x
removeNames' ctx recCtx (AST.CoreCons i es) = Cons i (fmap (removeNames' ctx recCtx) es)
removeNames' _ _ (AST.Cons t x) = error ("Invalid core: " <> show (AST.Cons t x))
removeNames' ctx recCtx (AST.Tuple es) = Tuple (fmap (removeNames' ctx recCtx) es)
removeNames' ctx recCtx (AST.GetT i es) = Get i (removeNames' ctx recCtx es)
removeNames' ctx recCtx (AST.UpdateT i modify es) = Update i (removeNames' ctx recCtx modify) (removeNames' ctx recCtx es)
removeNames' ctx recCtx (AST.CoreCase e es) = Case (removeNames' ctx recCtx e) (fmap (removeNames' ctx recCtx) es)
removeNames' _ _ (AST.Case e es) = error ("Invalid core: " <> show (AST.Case e es))
removeNames' ctx recCtx (AST.Let x s e) = Let x (removeNames' ctx recCtx s) (removeNames' (x : ctx) recCtx e)
removeNames' ctx recCtx (AST.LetRec x s e) = LetRec x (removeNames' ctx (x : recCtx) s) (removeNames' (x : ctx) recCtx e)
removeNames' ctx recCtx (AST.Prim1 x e) = Prim1 (readPrim1 x) (removeNames' ctx recCtx e)
removeNames' ctx recCtx (AST.Prim2 x e1 e2) = Prim2 (readPrim2 x) (removeNames' ctx recCtx e1) (removeNames' ctx recCtx e2)
removeNames' ctx recCtx (AST.IOPrim x es) = IOPrim (readIOPrim x) (fmap (removeNames' ctx recCtx) es)
removeNames' ctx recCtx (AST.Bind e1 e2) = Bind (removeNames' ctx recCtx e1) (removeNames' ctx recCtx e2)
removeNames' ctx recCtx (AST.Pure e) = Pure (removeNames' ctx recCtx e)
