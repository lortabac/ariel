module Ariel.Syntax.Canonical (makeLetRecs) where

import Ariel.Syntax.AST
import Ariel.Syntax.Types
import Control.Monad.Writer.Strict
import Data.Monoid (Any)

makeLetRecs :: Expr -> Expr
makeLetRecs (Let x s e) = case runWriter (makeRecVars x s) of
  (s', Any True) -> LetRec x s' (makeLetRecs e)
  _ -> Let x s (makeLetRecs e)
makeLetRecs (Var x) = Var x
makeLetRecs (RecVar x) = RecVar x
makeLetRecs (Lam x e) = Lam x (makeLetRecs e)
makeLetRecs (App e1 e2) = App (makeLetRecs e1) (makeLetRecs e2)
makeLetRecs (Int x) = Int x
makeLetRecs (Double x) = Double x
makeLetRecs (Text x) = Text x
makeLetRecs (CoreCons i es) = CoreCons i (fmap makeLetRecs es)
makeLetRecs (Cons t es) = Cons t (fmap makeLetRecs es)
makeLetRecs (Tuple es) = Tuple (fmap makeLetRecs es)
makeLetRecs (GetT i t) = GetT i (makeLetRecs t)
makeLetRecs (UpdateT i f t) = UpdateT i f (makeLetRecs t)
makeLetRecs (CoreCase e es) = CoreCase (makeLetRecs e) (fmap makeLetRecs es)
makeLetRecs (Case e es) = Case (makeLetRecs e) (fmap makeLetRecs es)
makeLetRecs (LetRec x s e) = LetRec x (fst (runWriter (makeRecVars x s))) (makeLetRecs e)
makeLetRecs (Prim p es) = Prim p (fmap makeLetRecs es)
makeLetRecs (IOPrim p es) = IOPrim p (fmap makeLetRecs es)
makeLetRecs (Bind e1 e2) = Bind (makeLetRecs e1) (makeLetRecs e2)
makeLetRecs (Pure e) = Pure (makeLetRecs e)

makeRecVars :: Name -> Expr -> Writer Any Expr
makeRecVars n1 (Var n2) =
  if n1 == n2
    then tell (Any True) >> pure (RecVar n1)
    else pure (Var n2)
makeRecVars _ (RecVar x) = pure $ RecVar x
makeRecVars n1 (Lam n2 e) =
    if n1 == n2
        then pure $ Lam n2 e
        else Lam n2 <$> makeRecVars n1 e
makeRecVars n (App e1 e2) = App <$> makeRecVars n e1 <*> makeRecVars n e2
makeRecVars _ (Int x) = pure $ Int x
makeRecVars _ (Double x) = pure $ Double x
makeRecVars _ (Text x) = pure $ Text x
makeRecVars n (CoreCons i es) = CoreCons i <$> traverse (makeRecVars n) es
makeRecVars n (Cons t es) = Cons t <$> traverse (makeRecVars n) es
makeRecVars n (Tuple es) = Tuple <$> traverse (makeRecVars n) es
makeRecVars n (GetT i t) = GetT i <$> makeRecVars n t
makeRecVars n (UpdateT i f t) = UpdateT i f <$> makeRecVars n t
makeRecVars n (CoreCase e es) = CoreCase <$> makeRecVars n e <*> traverse (makeRecVars n) es
makeRecVars n (Case e es) = Case <$> makeRecVars n e <*> traverse (makeRecVars n) es
makeRecVars n1 (Let n2 s e) =
  if n1 == n2
    then pure $ Let n2 s e
    else Let n2 <$> makeRecVars n1 s <*> makeRecVars n1 e
makeRecVars n1 (LetRec n2 s e) =
  if n1 == n2
    then pure $ Let n2 s e
    else LetRec n2 <$> makeRecVars n1 s <*> makeRecVars n1 e
makeRecVars n (Prim p es) = Prim p <$> traverse (makeRecVars n) es
makeRecVars n (IOPrim p es) = IOPrim p <$> traverse (makeRecVars n) es
makeRecVars n (Bind e1 e2) = Bind <$> makeRecVars n e1 <*> makeRecVars n e2
makeRecVars n (Pure e) = Pure <$> makeRecVars n e
