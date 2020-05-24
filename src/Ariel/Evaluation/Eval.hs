module Ariel.Evaluation.Eval where

import Ariel.Evaluation.Nameless
import Ariel.Evaluation.Prim
import Ariel.Evaluation.Types
import qualified Ariel.Syntax.AST as AST
import Ariel.Syntax.Canonical
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
      (Tuple es, Abs f) ->
        let modifyV v = do
              el <- MV.read v (i - 1)
              el' <- unsafeIOToST $ eval env (App (Abs f) el)
              MV.write v (i - 1) el'
         in pure $ Tuple (V.modify modifyV es)
      _ -> error ("Invalid Update: " <> show e)
  | otherwise = error ("Invalid Update: " <> show e)
eval _ (Abs e) = pure $ Abs e
eval env e@App {} = snd <$> evalApp env e
eval env (Let e1 e2) = do
  e1' <- eval env e1
  env' <- extendEnv e1' env
  eval env' e2
eval env (LetRec e1 e2) = do
  env' <- extendRecEnv (Rec env e1) env
  e1' <- eval env' e1
  env'' <- extendEnv e1' env
  eval env'' e2
eval env (Var i) = eval env =<< lookupEnv i env
eval env (RecVar i) = do
  e <- lookupRecEnv i env
  case e of
    Rec env' e' -> eval env' e'
    t -> error ("Invalid RecVar: " <> show t)
eval _ (Rec env e) = eval env e
eval env (Prim p args) = evalPrim p <$> traverse (eval env) args
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

-- | Evaluate an application
--   Applications are treated specially because they modify the environment
evalApp :: Env -> Expr -> IO (Env, Expr)
evalApp env (App e1 e2) = do
  e1' <- evalApp env e1
  case e1' of
    (env', Abs t) -> do
      e2' <- eval env' e2
      env'' <- extendEnv e2' env'
      t' <- eval env'' t
      pure (env'', t')
    (_, e) -> error ("Invalid applicand: " <> show e)
evalApp env e = do
  e' <- eval env e
  pure (env, e')

-- | Convenience function to help testing evaluation directly on Ariel expressions
evalNamed :: AST.Expr -> IO Expr
evalNamed e = do
  env <- emptyEnv
  eval env (removeNames (makeLetRecs e))
