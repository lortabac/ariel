{-# LANGUAGE OverloadedStrings #-}

module Ariel.Runtime.Purify where

import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Runtime.Prim
import Ariel.Runtime.Types
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

purify ::
  Defs ->
  Expr ->
  (PExpr, Vector PExpr)
purify defs expr = (rtExpr, rtGlobals)
  where
    index = globalIndex (globals defs)
    rtExpr = purify' index defs expr
    rtGlobals = compileGlobals index defs

purify' ::
  Map QName Int ->
  Defs ->
  Expr ->
  PExpr
purify' _ _ (Int i) = PConst (VInt i)
purify' _ _ (String s) = PConst (VString s)
purify' ix defs (Con qn tag) = purify' ix defs $ churchCon (sumTypes defs) qn tag
purify' ix defs (Case e es) = purify' ix defs $ churchCase e es
purify' _ _ (Var name) = PVar name
purify' index _ (Global qn) = PConst (VGlobal (index ! qn))
purify' ix defs (Lam name e) = PAbs name (purify' ix defs e)
purify' ix defs (App e arg) = PApp (purify' ix defs e) (purify' ix defs arg)
purify' ix defs (Let name body e) = purify' ix defs (App (Lam name e) body)
purify' _ _ (Prim p) = lookupPrim p
purify' _ _ e = error ("Not supported yet: " ++ show e)

compileGlobals :: Map QName Int -> Defs -> Vector PExpr
compileGlobals index defs = fmap (purify' index defs) $ V.fromList $ Map.elems (globals defs)

globalIndex :: Map QName Expr -> Map QName Int
globalIndex = Map.fromList . flip zip [0 ..] . Map.keys

getIndex :: QName -> Tag -> Map QName (Set Tag) -> Int
getIndex qn tag st = Set.findIndex tag (st ! qn)

churchCon :: Map QName (Map Tag Int) -> QName -> Tag -> Expr
churchCon st qn (Tag name) = foldr Lam (foldr stepVariants app (Map.keys variants)) args
  where
    variants = st ! qn
    conArity = variants ! Tag name
    stepVariants (Tag n) = Lam n
    args = map (\n -> "x" <> tshow n) [1 .. conArity]
    app = foldl App (Var name) $ map Var args

churchCase :: Expr -> Map Tag Expr -> Expr
churchCase e eqs = foldl1 App (e : Map.elems eqs)

tshow :: Show a => a -> Text
tshow = T.pack . show
