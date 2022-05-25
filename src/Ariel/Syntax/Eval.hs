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

data Outcome
  = ExprOutcome ByteString
  | DeclOutcome QName Core.Ty Core.Defs
  deriving (Eq, Show)

runArielStr :: Map Text (Set Name) -> Core.Defs -> ByteString -> IO (Either [Text] Outcome)
runArielStr ns defs exprOrDecl = case decode exprOrDecl of
  Right (DOEExpr parsedExpr) -> do
    res <- runAriel ns defs parsedExpr
    case res of
      Right e -> pure $ Right $ ExprOutcome (encodeOrDie e)
      Left errs -> pure $ Left $ map showTCMessageWithPos (toList errs)
  Right (DOEDecl decl) -> case desugarDecl "user" ns decl of
    cDecl@(Core.Decl _ qname cExpr) -> do
      case typecheckCore defs cExpr of
        Right ty ->
          let newDefs = defsInsertDecl cDecl defs
           in pure $ Right $ DeclOutcome qname ty newDefs
        Left errs -> pure $ Left $ map showTCMessageWithPos (toList errs)
  Left parsingErr -> pure $ Left ["Parsing error: " <> T.pack parsingErr]

runAriel :: Map Text (Set Name) -> Core.Defs -> Expr -> IO (Either (Set TCMessage) Expr)
runAriel ns defs expr = case typecheckCore defs cExpr of
  Right _ ->
    let (pExpr, pGlobals) = compile defs cExpr
        iGlobals = fmap removeNames pGlobals
        iExpr = removeNames pExpr
     in Right . sweetenExpr . readBackV <$> run iGlobals mempty iExpr
  Left err -> pure $ Left err
  where
    cExpr = desugarExpr ns expr

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

typecheckArielStr :: Map Text (Set Name) -> Core.Defs -> ByteString -> Either [Text] ByteString
typecheckArielStr ns defs expr = tc
  where
    tc = do
      ast <- first (pure . T.pack) $ decode expr
      ty <- first (map showTCMessageWithPos . toList) $ typecheckAriel ns defs ast
      pure $ encodeOrDie ty

typecheckAriel :: Map Text (Set Name) -> Core.Defs -> Expr -> Either (Set TCMessage) Ty
typecheckAriel ns defs expr = sweetenTy . readBackTy <$> typecheckCore defs (desugarExpr ns expr)

typecheckCore :: Core.Defs -> Core.Expr -> Either (Set TCMessage) Core.Ty
typecheckCore defs = typecheckWithGlobalCtx gCtx
  where
    gCtx = buildGlobalCtx (Core._globals defs)

buildGlobalCtx :: Map QName Core.Expr -> GlobalCtx Core.Ty
buildGlobalCtx gs = foldr (\(qname, e) acc -> addToGCtx qname e acc) emptyGlobalCtx globalDefs
  where
    globalDefs = Map.toList gs
    addToGCtx qname e acc = case typecheckWithGlobalCtx acc e of
      Right ty -> extendGlobalCtx qname ty acc
      Left _ -> error "Type error"

defsInsertDecl :: Core.Decl -> Core.Defs -> Core.Defs
defsInsertDecl (Core.Decl _ qname expr) =
  over Core.globals (Map.insert qname expr)
