module Ariel.TC.Builder where

import Ariel.Syntax.Ty
import Ariel.Syntax.Types
import Ariel.TC.Check
import Control.Unification

tInt :: TyTerm
tInt = UTerm TInt

tText :: TyTerm
tText = UTerm TText

tDouble :: TyTerm
tDouble = UTerm TDouble

(-->) :: TyTerm -> TyTerm -> TyTerm
(-->) t1 t2 = UTerm (TArr t1 t2)

infixr 1 -->

tVar :: TyVar -> TyTerm
tVar = UTerm . TVar

forall :: [TyVar] -> TyTerm -> TyTerm
forall vars t = UTerm (Forall vars t)
