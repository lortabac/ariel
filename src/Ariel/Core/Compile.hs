{-# LANGUAGE OverloadedStrings #-}

module Ariel.Core.Compile where

import Ariel.Common.Types
import Ariel.Core.Types (Defs (..))
import qualified Ariel.Core.Types as Core
import qualified Ariel.Runtime.Types as RT
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

compileCore ::
  Defs ->
  Core.Expr ->
  (RT.Expr, Vector RT.Expr)
compileCore defs expr = (rtExpr, rtGlobals)
  where
    index = globalIndex (globals defs)
    rtExpr = compileCore' index defs mempty expr
    rtGlobals = compileGlobals index defs

compileCore' ::
  Map QName Int ->
  Defs ->
  Map Text RT.Expr ->
  Core.Expr ->
  RT.Expr
compileCore' _ _ _ (Core.Int i) = RT.Int i
compileCore' _ _ _ (Core.String s) = RT.String s
compileCore' ix defs env (Core.Con qn tag es) = RT.Con tag (getIndex qn tag (sumTypes defs)) $ fmap (compileCore' ix defs env) es
compileCore' _ _ env (Core.Var name) = env ! name
compileCore' index _ _ (Core.Global qn) = RT.Global (index ! qn)
compileCore' ix defs env (Core.Lam [name] e) = RT.Lam1 (\x -> compileCore' ix defs (Map.insert name x env) e)
compileCore' ix defs env (Core.Lam [n1, n2] e) = RT.Lam2 (\x1 x2 -> compileCore' ix defs (Map.insert n1 x1 $ Map.insert n2 x2 env) e)
compileCore' ix defs env (Core.Lam [n1, n2, n3] e) = RT.Lam3 (\x1 x2 x3 -> compileCore' ix defs (Map.insert n1 x1 $ Map.insert n2 x2 $ Map.insert n3 x3 env) e)
compileCore' ix defs env (Core.Fix name e) = RT.Fix (\x -> compileCore' ix defs (Map.insert name x env) e)
compileCore' ix defs env (Core.App e [arg]) = RT.App1 (compileCore' ix defs env e) (compileCore' ix defs env arg)
compileCore' ix defs env (Core.App e [arg1, arg2]) = RT.App2 (compileCore' ix defs env e) (compileCore' ix defs env arg1) (compileCore' ix defs env arg2)
compileCore' ix defs env (Core.App e [arg1, arg2, arg3]) = RT.App3 (compileCore' ix defs env e) (compileCore' ix defs env arg1) (compileCore' ix defs env arg2) (compileCore' ix defs env arg3)
compileCore' ix defs env (Core.Case e es) = RT.Case (compileCore' ix defs env e) $ Vector.fromList $ Map.elems $ fmap (compileCore' ix defs env) es
compileCore' ix defs env (Core.Let name body e) = compileCore' ix defs env (Core.App (Core.Lam [name] e) [body])
compileCore' ix defs env (Core.Prim2 p e1 e2) = RT.Prim2 p (compileCore' ix defs env e1) (compileCore' ix defs env e2)
compileCore' ix defs env (Core.IOPrim p) = RT.IOPrim $ fmap (compileCore' ix defs env) p
compileCore' ix defs env (Core.Bind e k) = RT.Bind (compileCore' ix defs env e) (compileCore' ix defs env k)
compileCore' ix defs env (Core.Pure e) = RT.Pure (compileCore' ix defs env e)
compileCore' _ _ _ e = error ("Not supported yet: " ++ show e)

compileGlobals :: Map QName Int -> Defs -> Vector RT.Expr
compileGlobals index defs = fmap (compileCore' index defs mempty) $ Vector.fromList $ Map.elems (globals defs)

globalIndex :: Map QName Core.Expr -> Map QName Int
globalIndex = Map.fromList . flip zip [0 ..] . Map.keys

getIndex :: QName -> Tag -> Map QName (Set Tag) -> Int
getIndex qn tag st = Set.findIndex tag (st ! qn)

decompileRT :: RT.Expr -> Core.Expr
decompileRT (RT.Int i) = Core.Int i
decompileRT (RT.String s) = Core.String s
decompileRT (RT.Con tag _ es) = Core.Con (QName "_" "_") tag $ map decompileRT es
decompileRT _ = error "Cannot decompile"
