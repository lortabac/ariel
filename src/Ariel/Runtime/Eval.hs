module Ariel.Runtime.Eval where

import Ariel.Runtime.Prim
import Ariel.Runtime.Types
import Data.Function (fix)
import Data.Vector (Vector, (!))

eval :: Vector Expr -> Expr -> IO Expr
eval _ (Int i) = pure (Int i)
eval _ (String s) = pure (String s)
eval globals (Con tag i e) = Con tag i <$> traverse (eval globals) e
eval globals (Global i) = eval globals (globals ! i)
eval _ (Lam1 f) = pure (Lam1 f)
eval _ (Lam2 f) = pure (Lam2 f)
eval _ (Lam3 f) = pure (Lam3 f)
eval globals (App1 f e) = do
    e' <- eval globals e
    case f of
        Lam1 l -> eval globals (l e')
        Lam2 l -> eval globals (Lam1 (l e'))
        Lam3 l -> eval globals (Lam2 (l e'))
        f' -> do
            f'' <- eval globals f'
            eval globals (App1 f'' e')
eval globals (App2 f e1 e2) = do
    e1' <- eval globals e1
    e2' <- eval globals e2
    case f of
        Lam2 l -> eval globals (l e1' e2')
        Lam3 l -> eval globals (Lam1 (l e1' e2'))
        f' -> do
            f'' <- eval globals f'
            eval globals (App2 f'' e1' e2')
eval globals (App3 f e1 e2 e3) = do
    e1' <- eval globals e1
    e2' <- eval globals e2
    e3' <- eval globals e3
    case f of
        Lam3 l -> eval globals (l e1' e2' e3')
        f' -> do
            f'' <- eval globals f'
            eval globals (App3 f'' e1' e2' e3')
eval _ (Fix f) = pure (fix f)
eval globals (Case e es) = do
    e' <- eval globals e
    case e' of
        Con _ i args -> do
            eval globals $ foldl App1 (es ! i) args
        _ -> error "Invalid case"
eval globals (Prim2 p e1 e2) = do
    e1' <- eval globals e1
    e2' <- eval globals e2
    pure $ evalPrim2 p e1' e2'
eval _ (IOPrim p) = pure (IOPrim p)
eval _ (Bind e k) = pure (Bind e k)
eval _ (Pure e) = pure (Pure e)
