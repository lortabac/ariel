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
eval :: Env -> Env -> Expr -> IO Expr
eval _ _ (Int x) = pure $ Int x
eval _ _ (Double x) = pure $ Double x
eval _ _ (Text x) = pure $ Text x
eval env renv (Cons i es) = Cons i <$> traverse (eval env renv) es
eval env renv (Tuple es) = Tuple <$> traverse (eval env renv) es
eval env renv e@(Get (TupleIx i) t)
  | i >= 1 = do
    t' <- eval env renv t
    case t' of
      Tuple es -> pure (es V.! (i - 1))
      _ -> error ("Invalid Get: " <> show e)
  | otherwise = error ("Invalid Get: " <> show e)
eval env renv e@(Update (TupleIx i) modify t)
  | i >= 1 = do
    t' <- eval env renv t
    modify' <- eval env renv modify
    case (t', modify') of
      (Tuple es, clos@Clos {}) ->
        let modifyV v = do
              el <- MV.read v (i - 1)
              el' <- unsafeIOToST $ eval env renv (App clos el)
              MV.write v (i - 1) el'
         in pure $ Tuple (V.modify modifyV es)
      _ -> error ("Invalid Update: " <> show e)
  | otherwise = error ("Invalid Update: " <> show e)
eval env _ (Abs x e) = pure $ Clos x e (extendEnv Undefined env)
eval _ _ (Clos x e env) = pure $ Clos x e env
eval env renv (App e1 e2) = do
  e1' <- eval env renv e1
  case e1' of
    Clos _ t lamEnv -> do
      e2' <- eval env renv e2
      let env' = setEnvHead e2' lamEnv
      eval env' renv t
    e1'' -> error ("Invalid App: " <> show (App e1'' e2))
eval env renv (Let _ e1 e2) = do
  e1' <- eval env renv e1
  let env' = extendEnv e1' env
  eval env' renv e2
eval env renv (LetRec _ e1 e2) = do
  e1' <- eval env renv e1
  let env' = extendEnv e1' env
      renv' = extendEnv (RecClos e1' renv) renv
  eval env' renv' e2
eval env _ (RecClos e renv) = eval env renv e
eval env renv (Var _ i) = eval env renv (lookupEnv i env)
eval env renv (RecVar _ i) =
    case lookupEnv i renv of
      RecClos e renv' -> eval env renv' e
      _ -> error "Invalid RecVar"
eval env renv (Prim1 p e) = evalPrim1 p <$> eval env renv e
eval env renv (Prim2 p e1 e2) = evalPrim2 p <$> eval env renv e1 <*> eval env renv e2
eval env renv (IOPrim p args) = IOPrim p <$> traverse (eval env renv) args
eval env renv (Pure e) = Pure <$> eval env renv e
eval env renv (Bind e1 e2) = Bind <$> eval env renv e1 <*> eval env renv e2
eval env renv e@(Case p es) = do
  p' <- eval env renv p
  case p' of
    Cons (ConsIx i) args -> do
      lam <- eval env renv (es V.! i)
      eval env renv (foldl App lam args)
    _ -> error ("Invalid Case: " <> show e)
eval _ _ Undefined = error "Cannot evaluate Undefined"

-- | Convenience function to help testing evaluation directly on Ariel expressions
evalNamed :: AST.Expr -> IO Expr
evalNamed e = eval emptyEnv emptyEnv (removeNames (makeLetRecs e))
