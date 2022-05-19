module Ariel.TC.Prim where

import Ariel.Common.Prim
import Ariel.Core.Types

primTy :: Prim a -> ([Ty], Ty)
primTy Plus {} = ([TInt, TInt], TInt)
primTy Minus {} = ([TInt, TInt], TInt)
primTy Eq {} = ([TInt, TInt], TBool)
primTy Lt {} = ([TInt, TInt], TBool)
