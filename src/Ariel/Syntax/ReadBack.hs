{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Ariel.Syntax.ReadBack where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Ariel.Common.Types
import qualified Ariel.Core.Types as Core
import Ariel.Runtime.Env
import Ariel.Runtime.Types
import Ariel.Syntax.Types
import Control.Applicative ((<|>))
import Data.Bifunctor
import Data.Foldable (asum, toList)
import Data.Maybe (mapMaybe)
import GHC.Exts (isTrue#)
import GHC.Types (Int (..))

readBackV :: Value -> Expr
readBackV (VInt i) = Int (I# i)
readBackV (VString s) = String s
readBackV (VBool i) = if isTrue# i then T else F
readBackV (VClos env name e) = readBackEnv 1 env e $ Lam [name] (readBackI e)
readBackV (VDummyClos env name e) = readBackEnv 0 env e $ Lam [name] (readBackI e)
readBackV (VIOPrim p) = readBackVIOPrim p
readBackV (VBindIO e1 e2) = BindIO (readBackV e1) (readBackV e2)

readBackEnv :: Int -> Env Value -> IExpr -> Expr -> Expr
readBackEnv i env e1 e2 = case restoreEnv i env e1 of
  [] -> e2
  decls -> Let decls e2

restoreEnv :: Int -> Env Value -> IExpr -> [LetDecl]
restoreEnv i env = map (letDeclFromPair . fmap readBackV) . restoreNames i env

restoreNames :: Int -> Env Value -> IExpr -> [(Name, Value)]
restoreNames i env e = mapMaybe (\(mn, v) -> fmap (,v) mn) $ findNames i env e

findNames :: Int -> Env Value -> IExpr -> [(Maybe Name, Value)]
findNames i env e = first (`findNameI` e) <$> zip [i ..] (toList env)

findNameV :: Int -> Value -> Maybe Name
findNameV _ VInt {} = Nothing
findNameV _ VString {} = Nothing
findNameV _ VBool {} = Nothing
findNameV i (VClos _ _ e) = findNameI (i + 1) e
findNameV i (VDummyClos _ _ e) = findNameI i e
findNameV i (VIOPrim p) = asum $ map (findNameV i) (toList p)
findNameV i (VBindIO e1 e2) = findNameV i e1 <|> findNameV i e2

findNameI :: Int -> IExpr -> Maybe Name
findNameI i (IConst v) = findNameV i v
findNameI _ (IGlobal _) = Nothing
findNameI i (IVar name j)
  | i == j = Just name
  | otherwise = Nothing
findNameI i (IAbs _ e) = findNameI (i + 1) e
findNameI i (IDummyAbs _ e) = findNameI i e
findNameI i (IApp e1 e2) = findNameI i e1 <|> findNameI i e2
findNameI i (IPrim p) = asum $ map (findNameI i) (toList p)
findNameI i (IIOPrim p) = asum $ map (findNameI i) (toList p)
findNameI i (IIf c t f) = findNameI i c <|> findNameI i t <|> findNameI i f
findNameI i (IBindIO e1 e2) = findNameI i e1 <|> findNameI i e2
findNameI i (IFix e) = findNameI i e

readBackI :: IExpr -> Expr
readBackI (IConst v) = readBackV v
readBackI (IGlobal _) = error "Unimplemented"
readBackI (IVar name _) = Var name
readBackI (IAbs name e) = Lam [name] $ readBackI e
readBackI (IDummyAbs name e) = Lam [name] $ readBackI e
readBackI (IApp e1 e2) = App [readBackI e1, readBackI e2]
readBackI (IPrim p) = readBackPrim p
readBackI (IIOPrim p) = readBackIOPrim p
readBackI (IIf c t f) = If (readBackI c) (readBackI t) (readBackI f)
readBackI (IBindIO e1 e2) = BindIO (readBackI e1) (readBackI e2)
readBackI (IFix e) = Fix (readBackI e)

readBackPrim :: Prim IExpr -> Expr
readBackPrim (Eq e1 e2) = Prim "=" [readBackI e1, readBackI e2]
readBackPrim (Plus e1 e2) = Prim "+" [readBackI e1, readBackI e2]
readBackPrim (Minus e1 e2) = Prim "-" [readBackI e1, readBackI e2]
readBackPrim (Lt e1 e2) = Prim "<" [readBackI e1, readBackI e2]

readBackIOPrim :: IOPrim IExpr -> Expr
readBackIOPrim (Pure e) = IOPrim "pure" [readBackI e]
readBackIOPrim ReadLine = IOPrim "read-line" []
readBackIOPrim (Display e) = IOPrim "display" [readBackI e]

readBackVIOPrim :: IOPrim Value -> Expr
readBackVIOPrim (Pure e) = IOPrim "pure" [readBackV e]
readBackVIOPrim ReadLine = IOPrim "read-line" []
readBackVIOPrim (Display e) = IOPrim "display" [readBackV e]

readBackTy :: Core.Ty -> Ty
readBackTy (Core.TCon name) = TSym name
readBackTy (Core.TApp t1 t2) = TApp [readBackTy t1, readBackTy t2]
readBackTy (Core.TVar v) = TSym (unTyVar v)
readBackTy (Core.Forall vars t) = Forall vars (readBackTy t)
readBackTy (Core.Metavar _) = error "Can't readBackTy"
