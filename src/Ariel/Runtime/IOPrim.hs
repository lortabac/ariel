{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Runtime.IOPrim where

import Ariel.Common.IOPrim
import Ariel.Runtime.Types
import qualified Data.Text.IO as T

runIOPrim :: IOPrim Value -> IO Value
runIOPrim (Pure x) = pure x
runIOPrim ReadLine = VString <$> T.getLine
runIOPrim (Display (VString s)) = VInt 0# <$ T.putStr s
runIOPrim p = error ("Invalid io-prim application: " <> show p)
