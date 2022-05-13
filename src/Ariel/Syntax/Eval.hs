module Ariel.Syntax.Eval where

import Ariel.Common.Types
import Ariel.Core.Compile
import Ariel.Prelude
import Ariel.Runtime.Eval (eval, run)
import Ariel.Runtime.Nameless
import Ariel.Syntax.Desugar
import Ariel.Syntax.Errors
import Ariel.Syntax.ReadBack
import Ariel.Syntax.Sweeten
import Ariel.Syntax.Types
import Ariel.TC.Check
import Ariel.TC.Types
import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Language.SexpGrammar (decode, encodePretty)

runArielStr :: Defs -> ByteString -> IO ByteString
runArielStr defs expr = encodeOrDie <$> runAriel defs (decodeOrDie expr)
  where
    decodeOrDie e = case decode e of
      Right r -> r
      Left err -> error ("Parsing error: " ++ err)
    encodeOrDie e = case encodePretty e of
      Right r -> r
      Left err -> error ("Encoding error: " ++ err)

runAriel :: Defs -> Expr -> IO Expr
runAriel defs expr = sweetenExpr . readBackV <$> run iGlobals mempty iExpr
  where
    cDefs = desugarDefs defs
    cExpr = desugarExpr mempty expr
    (pExpr, pGlobals) = compile cDefs cExpr
    iGlobals = fmap removeNames pGlobals
    iExpr = removeNames pExpr

evalArielStr :: Defs -> ByteString -> ByteString
evalArielStr defs expr = encodeOrDie $ evalAriel defs (decodeOrDie expr)
  where
    decodeOrDie e = case decode e of
      Right r -> r
      Left err -> error ("Parsing error: " ++ err)
    encodeOrDie e = case encodePretty e of
      Right r -> r
      Left err -> error ("Encoding error: " ++ err)

evalAriel :: Defs -> Expr -> Expr
evalAriel defs expr = sweetenExpr . readBackV $ eval iGlobals mempty iExpr
  where
    cDefs = desugarDefs defs
    cExpr = desugarExpr mempty expr
    (pExpr, pGlobals) = compile cDefs cExpr
    iGlobals = fmap removeNames pGlobals
    iExpr = removeNames pExpr

typecheckArielStr :: Map Text (Set Name) -> ByteString -> Either [Text] ByteString
typecheckArielStr ns expr = tc
  where
    tc = do
      ast <- first (pure . T.pack) $ decode expr
      ty <- first (map showTCError . toList) $ typecheckAriel ns ast
      first (pure . T.pack) $ encodePretty ty

typecheckAriel :: Map Text (Set Name) -> Expr -> Either (Set TCError) Ty
typecheckAriel ns expr = sweetenTy . readBackTy <$> typecheck (desugarExpr ns expr)
