{-# LANGUAGE OverloadedStrings #-}

module Ariel.Syntax.Eval where

import Ariel.Common.Types
import Ariel.Core.Compile
import qualified Ariel.Core.Types as Core
import Ariel.Prelude
import Ariel.Runtime.Eval (eval, run)
import Ariel.Runtime.Nameless
import Ariel.Syntax.Desugar
import Ariel.Syntax.Errors
import Ariel.Syntax.ReadBack
import Ariel.Syntax.Sweeten
import Ariel.Syntax.Types
import Ariel.TC.Check
import Ariel.TC.Context
import Ariel.TC.Types
import Control.Lens (over)
import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.SexpGrammar (SexpIso, decode, encodePretty)

data TCCtx = TCCtx
  { names    :: Map Text (Set Name)
  , globals  :: Map QName Core.Expr
  , tyCtx    :: GlobalCtx Core.Ty
  , sumTypes :: Map QName (Map Tag [Core.Ty])
  } deriving (Eq, Show)

emptyTCCtx :: TCCtx
emptyTCCtx = TCCtx mempty mempty emptyGlobalCtx mempty

tcCtxToCoreDefs :: TCCtx -> Core.Defs
tcCtxToCoreDefs ctx = Core.Defs (globals ctx) (sumTypes ctx)


data Outcome
  = ExprOutcome ByteString
  | DeclOutcome QName Core.Ty Core.Expr
  deriving (Eq, Show)

runArielStr :: TCCtx -> ByteString -> IO (Either [Text] Outcome)
runArielStr ctx exprOrDecl = case decode exprOrDecl of
  Right (DOEExpr parsedExpr) -> do
    res <- runAriel ctx parsedExpr
    case res of
      Right e -> pure $ Right $ ExprOutcome (encodeOrDie e)
      Left errs -> pure $ Left $ map showTCMessageWithPos (toList errs)
  Right (DOEDecl decl) -> case desugarDecl "user" (names ctx) decl of
    Core.Decl _ qname cExpr -> do
      case typecheckCore ctx cExpr of
        Right ty -> pure $ Right $ DeclOutcome qname ty cExpr
        Left errs -> pure $ Left $ map showTCMessageWithPos (toList errs)
  Left parsingErr -> pure $ Left ["Parsing error: " <> T.pack parsingErr]

runAriel :: TCCtx -> Expr -> IO (Either (Set TCMessage) Expr)
runAriel ctx expr = case typecheckCore ctx cExpr of
  Right _ ->
    let (pExpr, pGlobals) = compile (tcCtxToCoreDefs ctx) cExpr
        iGlobals = fmap removeNames pGlobals
        iExpr = removeNames pExpr
     in Right . sweetenExpr . readBackV <$> run iGlobals mempty iExpr
  Left err -> pure $ Left err
  where
    cExpr = desugarExpr (names ctx) expr

evalArielStr :: Map Text (Set Name) -> Core.Defs -> ByteString -> ByteString
evalArielStr ns defs expr = encodeOrDie $ evalAriel ns defs (decodeOrDie expr)
  where
    decodeOrDie e = case decode e of
      Right r -> r
      Left err -> error ("Parsing error: " ++ err)

encodeOrDie :: SexpIso a => a -> ByteString
encodeOrDie e = case encodePretty e of
  Right r -> r
  Left err -> error ("Encoding error: " ++ err)

evalAriel :: Map Text (Set Name) -> Core.Defs -> Expr -> Expr
evalAriel ns defs expr = sweetenExpr . readBackV $ eval iGlobals mempty iExpr
  where
    cExpr = desugarExpr ns expr
    (pExpr, pGlobals) = compile defs cExpr
    iGlobals = fmap removeNames pGlobals
    iExpr = removeNames pExpr

typecheckArielStr :: TCCtx -> ByteString -> Either [Text] ByteString
typecheckArielStr ctx expr = tc
  where
    tc = do
      ast <- first (pure . T.pack) $ decode expr
      ty <- first (map showTCMessageWithPos . toList) $ typecheckAriel ctx ast
      pure $ encodeOrDie ty

typecheckAriel :: TCCtx -> Expr -> Either (Set TCMessage) Ty
typecheckAriel ctx expr = sweetenTy . readBackTy <$> typecheckCore ctx (desugarExpr (names ctx) expr)

typecheckCore :: TCCtx -> Core.Expr -> Either (Set TCMessage) Core.Ty
typecheckCore ctx = typecheckWithGlobalCtx gCtx
  where
    gCtx = buildGlobalCtx ctx

buildGlobalCtx :: TCCtx -> GlobalCtx Core.Ty
buildGlobalCtx ctx = foldr (\(qname, e) acc -> addToGCtx qname e acc) (tyCtx ctx) globalDefs
  where
    globalDefs = Map.toList $ globals ctx
    addToGCtx qname e acc = case typecheckWithGlobalCtx acc e of
      Right ty -> extendGlobalCtx qname ty acc
      Left err -> error ("Type error: " ++ show err)

defsInsertDecl :: Core.Decl -> Core.Defs -> Core.Defs
defsInsertDecl (Core.Decl _ qname expr) =
  over Core.globals (Map.insert qname expr)
