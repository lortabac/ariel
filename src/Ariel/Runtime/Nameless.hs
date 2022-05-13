module Ariel.Runtime.Nameless (removeNames) where

import Ariel.Common.Types
import Ariel.Runtime.Types
import Data.List (elemIndex)

removeNames :: PExpr -> IExpr
removeNames = removeNames' []

removeNames' :: [Name] -> PExpr -> IExpr
removeNames' _ (PConst c) = IConst c
removeNames' _ (PGlobal i) = IGlobal i
removeNames' ctx (PVar name) = case elemIndex name ctx of
  Just i -> IVar name i
  Nothing -> error ("Free variable " <> showName name)
removeNames' ctx (PAbs name e) = IAbs name (removeNames' (name : ctx) e)
removeNames' ctx (PWildAbs name e) = IDummyAbs name (removeNames' ctx e)
removeNames' ctx (PApp e1 e2) = IApp (removeNames' ctx e1) (removeNames' ctx e2)
removeNames' ctx (PPrim p) = IPrim (fmap (removeNames' ctx) p)
removeNames' ctx (PIOPrim p) = IIOPrim (fmap (removeNames' ctx) p)
removeNames' ctx (PIf e t f) = IIf (removeNames' ctx e) (removeNames' ctx t) (removeNames' ctx f)
removeNames' ctx (PBindIO e1 e2) = IBindIO (removeNames' ctx e1) (removeNames' ctx e2)
removeNames' ctx (PFix e) = IFix (removeNames' ctx e)
