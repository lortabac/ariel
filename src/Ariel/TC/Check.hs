{-# LANGUAGE OverloadedStrings #-}

module Ariel.TC.Check where

import Ariel.Common.Prim
import Ariel.Core.Types
import Ariel.Prelude
import Ariel.TC.Context
import Ariel.TC.Generalize
import Ariel.TC.Instantiate
import Ariel.TC.Prim
import Ariel.TC.Types
import Control.Lens
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
  arr <- newBoundMetavar (TArr e1Arr1 e1Arr2)
  whenSuccess_ t2 $ \t ->
    addEqConstr p e1Arr1 t
  whenSuccess_ t1 $ \t ->
    addEqConstr p arr t
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
      let (argTys, resTy) = primTy prim
          argsWithTys = zip args argTys
      inferedArgTys <- for argsWithTys $ \(expr, pTy) -> do
        inferedArgTy <- infer expr
        whenSuccess_ inferedArgTy $ \t ->
          addEqConstr p pTy t
        pure inferedArgTy
      pure $ resTy <$ sequenceA inferedArgTys
    Nothing -> ko $ TCMessage p (InvalidPrim name)
infer _ = error "Unsupported"
