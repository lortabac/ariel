module Ariel.TC.Prim where

import Ariel.Common.Prim
import Ariel.Core.Types
import Ariel.TC.Types

primTy :: Prim a -> InferM ([Ty], Ty)
primTy Plus {} = pure ([TInt, TInt], TInt)
primTy Minus {} = pure ([TInt, TInt], TInt)
primTy Equal {} = pure ([TInt, TInt], TBool)
primTy Lt {} = pure ([TInt, TInt], TBool)
