{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Core.Compile where

import Ariel.Common.IOPrim
import Ariel.Common.Prim
import Ariel.Common.Types
import Ariel.Core.Types
import Ariel.Runtime.Types
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Types (Int (..))
import Language.Sexp.Located (dummyPos)

compile ::
  Defs ->
  Expr ->
  (PExpr, Vector PExpr)
compile defs expr = (rtExpr, rtGlobals)
  where
    index = globalIndex (globals defs)
    rtExpr = compile' index defs expr
    rtGlobals = compileGlobals index defs

compile' ::
  Map QName Int ->
  Defs ->
  Expr ->
  PExpr
compile' _ _ (Int (I# i)) = PConst (VInt i)
compile' _ _ (String s) = PConst (VString s)
compile' _ _ (Bool b) = PConst (VBool (boolToInt# b))
compile' ix defs (Con qn tag) = compile' ix defs $ churchCon (sumTypes defs) qn tag
compile' ix defs (Case e es) = compile' ix defs $ churchCase e es
compile' _ _ (Var _ name) = PVar name
compile' index _ (Global qn) = PGlobal (index ! qn)
compile' ix defs (Lam _ name e) =
  if isWildcard name
    then PWildAbs name (compile' ix defs e)
    else PAbs name (compile' ix defs e)
compile' ix defs (App _ e arg) = PApp (compile' ix defs e) (compile' ix defs arg)
compile' ix defs (If _ e t f) = PIf (compile' ix defs e) (compile' ix defs t) (compile' ix defs f)
compile' ix defs (Let p name body e) = compile' ix defs (App p (Lam p name e) body)
compile' ix defs (BindIO _ e1 e2) = PBindIO (compile' ix defs e1) (compile' ix defs e2)
compile' ix defs (Fix _ e) = PFix (compile' ix defs e)
compile' ix defs (Ann _ e _) = compile' ix defs e
compile' ix defs (Prim _ name es) = PPrim $ readPrimOrDie name (map (compile' ix defs) es)
compile' ix defs (IOPrim _ name es) = PIOPrim $ readIOPrimOrDie name (map (compile' ix defs) es)

compileGlobals :: Map QName Int -> Defs -> Vector PExpr
compileGlobals index defs = fmap (compile' index defs) $ V.fromList $ map snd (globals defs)

globalIndex :: [(QName, Expr)] -> Map QName Int
globalIndex = Map.fromList . flip zip [0 ..] . map fst

getIndex :: QName -> Tag -> Map QName (Set Tag) -> Int
getIndex qn tag st = Set.findIndex tag (st ! qn)

churchCon :: Map QName (Map Tag [Ty]) -> QName -> Tag -> Expr
churchCon st qn (Tag name) = foldr (Lam dummyPos) (foldr stepVariants app (Map.keys variants)) args
  where
    variants = st ! qn
    conArity = length (variants ! Tag name)
    stepVariants (Tag n) = Lam dummyPos (Name n)
    args = map (\n -> Name ("x" <> tshow n)) [1 .. conArity]
    app = foldl (App dummyPos) (Var dummyPos (Name name)) $ map (Var dummyPos) args

churchCase :: Expr -> Map Tag Expr -> Expr
churchCase e eqs = foldl1 (App dummyPos) (e : Map.elems eqs)

isWildcard :: Name -> Bool
isWildcard (Name name) = T.head name == '_'

tshow :: Show a => a -> Text
tshow = T.pack . show
