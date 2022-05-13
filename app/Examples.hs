{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel.Syntax.Eval
import Data.ByteString.Lazy (ByteString)

exampleSumr :: IO ByteString
exampleSumr = runArielStr mempty expr
  where
    expr = "((named-lambda sumr (n) (if (,= n 0) 0 (,+ n (sumr (,- n 1))))) 10000000)"
