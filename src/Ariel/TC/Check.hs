{-# LANGUAGE OverloadedStrings #-}

module Ariel.TC.Check where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.TC.Context
import Ariel.TC.Generalize
import Ariel.TC.Instantiate
import Ariel.TC.IOPrim
import Ariel.TC.Kind
import Ariel.TC.Prim
import Ariel.TC.Types
import Control.Lens
import Logic.Unify (UnificationResult(..), subsumes)
import Validation

typecheck :: Expr -> Either (Set TCMessage) Ty
typecheck e = fst $
  runInferM $ do
    vT <- infer e
    vRes <- applyConstraints
    case (vT, vRes) of
      (Success t, Success _) -> do
        vT' <- applyBindingsV t
        case vT' of
          Success t' -> validationToEither <$> generalize t'
          Failure err -> pure $ Left err
      _ -> pure $ Left $ mconcat (failures [vT] ++ failures [vRes])

infer :: Expr -> InferM (Validation (Set TCMessage) Ty)
infer Int {} = ok $ TCon "Int"
infer String {} = ok $ TCon "String"
infer Bool {} = ok $ TCon "Bool"
infer (Var p name) = do
  maybeT <- asks (\ctx -> lookupLocalCtx name (ctx ^. localCtx))
  case maybeT of
    Just t -> instantiate t
    Nothing -> ko $ TCMessage p (OutOfScopeVar name)
infer (Global name) = do
  maybeT <- asks (\ctx -> lookupGlobalCtx name (ctx ^. globalCtx))
  case maybeT of
    Just t -> instantiate t
    Nothing -> error ("Not in scope: " ++ show name)
infer (Lam _ name e) = do
  varTy <- newMetavar
  bodyTy <- local (over localCtx (extendLocalCtx name varTy)) $ infer e
  pure $ TArr varTy <$> bodyTy
infer (App p e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  e1Arr1 <- newMetavar
  e1Arr2 <- newMetavar
  whenSuccess_ t2 $ \t ->
    addEqConstr p e1Arr1 t
  whenSuccess_ t1 $ \t ->
    addEqConstr p (TArr e1Arr1 e1Arr2) t
  pure $ (\_ _ -> e1Arr2) <$> t1 <*> t2
infer (Let _ name expr body) = do
  exprTy <- infer expr
  case exprTy of
    Success t -> do
      gexprTy <- generalize t
      case gexprTy of
        Success gt ->
          local (over localCtx (extendLocalCtx name gt)) $ infer body
        Failure errs -> pure $ Failure errs
    Failure errs -> pure $ Failure errs
infer (Prim p name args) = do
  let maybePrim = readPrim name args
  case maybePrim of
    Just prim -> do
      (argTys, resTy) <- primTy prim
      let argsWithTys = zip args argTys
      inferedArgTys <- for argsWithTys $ \(expr, pTy) -> do
        inferedArgTy <- infer expr
        whenSuccess_ inferedArgTy $ \t ->
          addEqConstr p pTy t
        pure inferedArgTy
      pure $ resTy <$ sequenceA inferedArgTys
    Nothing -> ko $ TCMessage p (InvalidPrim name)
infer (IOPrim p name args) = do
  let maybePrim = readIOPrim name args
  case maybePrim of
    Just prim -> do
      (argTys, resTy) <- ioPrimTy prim
      let argsWithTys = zip args argTys
      inferedArgTys <- for argsWithTys $ \(expr, pTy) -> do
        inferedArgTy <- infer expr
        whenSuccess_ inferedArgTy $ \t ->
          addEqConstr p pTy t
        pure inferedArgTy
      pure $ resTy <$ sequenceA inferedArgTys
    Nothing -> ko $ TCMessage p (InvalidPrim name)
infer (Fix p expr) = do
  exprT <- infer expr
  eArr1 <- newMetavar
  eArr2 <- newMetavar
  whenSuccess_ exprT $ \t -> do
    addEqConstr p (TArr eArr1 eArr2) t
    addEqConstr p eArr1 eArr2
  pure $ eArr1 <$ exprT
infer (Ann p expr ann) = do
  case kindCheck ann of
    Just Star -> do
      exprTy <- infer expr
      annTy <- instantiate ann
      case successes [exprTy, annTy] of
        [exprT, annT] -> do
          subCheck <- subsumes exprT annT
          case subCheck of
            Unified -> ok annT
            OccursFailure _ t -> ko $ TCMessage p (CyclicType t)
            UnificationFailure t1 t2 -> ko $ TCMessage p (TypeMismatch t1 t2)
            SubsumptionFailure t1 t2 -> ko $ TCMessage p (SubsumptionError t1 t2)
        _ -> pure $ Failure (mconcat (failures [exprTy, annTy]))
    _ -> ko $ TCMessage p (KindError ann)
infer (BindIO p e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2
  a <- newMetavar
  b <- newMetavar
  case successes [ty1, ty2] of
    [t1, t2] -> do
      addEqConstr p t1 (TIO a)
      addEqConstr p t2 (TArr a (TIO b))
      ok $ TIO b
    _ -> pure $ Failure (mconcat (failures [ty1, ty2]))
infer _ = error "Unsupported"
