module Ariel.TC.IOPrim where

import Ariel.Common.IOPrim
import Ariel.Core.Types
import Ariel.TC.Types

ioPrimTy :: IOPrim a -> InferM ([Ty], Ty)
ioPrimTy ReadLine = pure ([], TIO TString)
ioPrimTy Display {} = pure ([TString], TIO TInt)
ioPrimTy Pure {} = do
  a <- newMetavar
  pure ([a], TIO a)
