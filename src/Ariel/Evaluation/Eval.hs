module Ariel.Evaluation.Eval where

import Ariel.Evaluation.Nameless
import Ariel.Evaluation.Prim
import Ariel.Evaluation.Types
import qualified Ariel.Syntax.AST as AST
import Ariel.Syntax.Transform
import Ariel.Syntax.Types
import Control.Monad.ST.Unsafe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | Evaluate a nameless core expression
eval :: Env -> Expr -> IO Expr
eval _ (Int x) = pure $ Int x
eval _ (Double x) = pure $ Double x
eval _ (Text x) = pure $ Text x
eval env (Cons i es) = Cons i <$> traverse (eval env) es
eval env (Tuple es) = Tuple <$> traverse (eval env) es
eval env e@(Get (TupleIx i) t)
  | i >= 1 = do
    t' <- eval env t
    case t' of
      Tuple es -> pure (es V.! (i - 1))
      _ -> error ("Invalid Get: " <> show e)
  | otherwise = error ("Invalid Get: " <> show e)
eval env e@(Update (TupleIx i) modify t)
  | i >= 1 = do
    t' <- eval env t
    modify' <- eval env modify
    case (t', modify') of
      (Tuple es, clos@Clos {}) ->
        let modifyV v = do
              el <- MV.read v (i - 1)
              el' <- unsafeIOToST $ eval env (App clos el)
              MV.write v (i - 1) el'
         in pure $ Tuple (V.modify modifyV es)
      _ -> error ("Invalid Update: " <> show e)
  | otherwise = error ("Invalid Update: " <> show e)
eval env (Abs x e) = pure $ Clos x e (extendEnv Undefined env)
eval _ (Clos x e env) = pure $ Clos x e env
eval env (App e1 e2) = do
  e1' <- eval env e1
  case e1' of
    Clos _ t lamEnv -> do
      e2' <- eval env e2
      let env' = setEnvIndex 0 e2' lamEnv
      eval env' t
    e1'' -> error ("Invalid App: " <> show (App e1'' e2))
eval env (Let _ e1 e2) = do
  e1' <- eval env e1
  let env' = extendEnv e1' env
  eval env' e2
eval env (LetRec _ e1 e2) = do
  e1' <- eval env e1
  let e1'' = case e1' of
        Clos x e envir -> Clos x e (extendRecEnv e envir)
        e -> e
  let env' = extendEnv e1'' env
  eval env' e2
eval env (Var _ i) = eval env (lookupEnv i env)
eval env (RecVar _ i) = eval env (lookupRecEnv i env)
eval env (Prim1 p e) = evalPrim1 p <$> eval env e
eval env (Prim2 p e1 e2) = evalPrim2 p <$> eval env e1 <*> eval env e2
eval env (IOPrim p args) = IOPrim p <$> traverse (eval env) args
eval env (Pure e) = Pure <$> eval env e
eval env (Bind e1 e2) = Bind <$> eval env e1 <*> eval env e2
eval env e@(Case p es) = do
  p' <- eval env p
  case p' of
    Cons (ConsIx i) args -> do
      lam <- eval env (es V.! i)
      eval env (foldl App lam args)
    _ -> error ("Invalid Case: " <> show e)
eval _ Undefined = error "Cannot evaluate Undefined"

-- | Convenience function to help testing evaluation directly on Ariel expressions
evalNamed :: AST.Expr -> IO Expr
evalNamed e = eval emptyEnv (removeNames (makeLetRecs e))
