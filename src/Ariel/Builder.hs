module Ariel.Builder (
    (==>)
  , (===>)
  , (@@)
  , (=:)
  , ins
  , inrecs
  , (>>==)
  , tInt
  , tText
  , tDouble
  , (-->)
  , tVar
  , forall
  , unsafeFreeze
  , module Ariel.TC.Check
) where

import Ariel.Syntax.AST
import Ariel.Syntax.Ty
import Ariel.Syntax.Types
import Ariel.TC.Check
import Control.Unification
import Data.Functor.Fixedpoint
import Data.Maybe (fromJust)

-- | Convenience operator for lambdas
(==>) :: Name -> Expr -> Expr
(==>) = Lam

infixr 1 ==>

(===>) :: [Name] -> Expr -> Expr
(===>) = MultiLam

infixr 2 ===>

-- | Convenience operator for applications
(@@) :: Expr -> [Expr] -> Expr
(@@) = MultiApp

infixl 9 @@

(=:) :: Name -> Expr -> TermDecl
(=:) = TermDecl

infix 4 =:

-- | Convenience operator for let expressions
ins :: [TermDecl] -> Expr -> Expr
ins = MultiLet

infixr 2 `ins`

-- | Convenience operator for let rec expressions
inrecs :: [TermDecl] -> Expr -> Expr
inrecs = MultiLetRec

infixr 2 `inrecs`

-- | Convenience operator for bind
(>>==) :: Expr -> Expr -> Expr
(>>==) = Bind

infixl 1 >>==

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

unsafeFreeze :: Traversable t => UTerm t v -> Fix t
unsafeFreeze = fromJust . freeze
