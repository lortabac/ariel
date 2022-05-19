{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel.Syntax.Eval
import Data.ByteString.Lazy (ByteString)

exampleSumr :: IO ByteString
exampleSumr = runArielStr mempty expr
  where
    expr = "(let ([+ (lambda (x y) (#prim + x y))]) ((named-lambda sumr (n) (if (#prim = n 0) 0 (#prim + n (sumr (#prim - n 1))))) 10000000))"
