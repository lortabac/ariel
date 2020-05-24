{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Evaluation.IOPrim where

import Ariel.Evaluation.Types
import qualified Data.Text.IO as Text

runIOPrim :: IOPrim -> [Expr] -> IO Expr
runIOPrim WriteLn [Text x] = Text.putStrLn x >> pure (Tuple [])
runIOPrim ReadLine [] = Text <$> Text.getLine
runIOPrim p args = error ("Invalid IOPrim: " <> show (IOPrim p args))
