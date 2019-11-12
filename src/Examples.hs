{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Ariel

example_letIO :: IO ExprH
example_letIO =
  runCore emptyEnv $
    Let (Var "putStrLn") (IOPrim "putStrLn#") (Var "putStrLn" `App` String "hello world")

example_fixIO :: IO ExprH
example_fixIO =
  runCore emptyEnv $
    Fix (Var "rec") (IOPrim "putStr#" `App` String "hello" `Bind` Lam (Var "y") (Var "rec"))

example_letFixIO :: IO ExprH
example_letFixIO =
  runCore emptyEnv $
    Let
      (Var "putStr")
      (IOPrim "putStr#")
      (Fix (Var "rec") (Var "putStr" `App` String "hello" `Bind` Lam (Var "y") (Var "rec")))
