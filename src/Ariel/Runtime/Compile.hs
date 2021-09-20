module Ariel.Runtime.Compile (compile) where

import Ariel.Runtime.Types

compile :: PExpr -> MExpr
compile = checkNoVar . elim

elim :: PExpr -> PExpr
elim (PConst c) = PConst c
elim (PVar x) = PVar x
elim (PApp e1 e2) = PApp (elim e1) (elim e2)
elim (PAbs s (PVar s'))
  | s == s' = combI
  | otherwise = PApp combK (PVar s')
elim (PAbs _ (PConst c)) = PApp combK (PConst c)
elim (PAbs s (PApp e1 e2)) = preEvalApp $ PApp (PApp combS (elim (PAbs s e1))) (elim (PAbs s e2))
elim (PAbs s t@PAbs{}) = elim (PAbs s (elim t))

preEvalApp :: PExpr -> PExpr
preEvalApp (PApp e1 e2) = case (preEvalApp e1, preEvalApp e2) of
  (PConst (VFun f), PConst v) -> PConst (f v)
  (e1', e2') -> PApp e1' e2'
preEvalApp e = e

checkNoVar :: PExpr -> MExpr
checkNoVar (PConst c) = MConst c
checkNoVar (PApp e1 e2) = MApp (checkNoVar e1) (checkNoVar e2)
checkNoVar e = error ("Unexpected after elimination: " <> show e)

combI :: PExpr
combI = PConst $ VFun id

combK :: PExpr
combK = PConst $ VFun (VFun . const)

combS :: PExpr
combS = PConst $ VFun $ \(VFun f) -> VFun $ \(VFun g) -> VFun $
  \x ->
    case f x of
      VFun h -> h (g x)
      _ -> error "Can't apply combS"
