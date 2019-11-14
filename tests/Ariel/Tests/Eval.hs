{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Eval
  ( evalTests,
  )
where

import Ariel
import Data.Text (Text)
import Data.Vector (Vector)
import Test.Tasty
import Test.Tasty.HUnit

defs :: Env (Expr 'Core)
defs = [("id", id_), ("const", const_)]

env :: Env ExprH
env = mapToExprH [] defs

id_ :: Expr 'Core
id_ = Lam (Var "x") (Var "x")

const_ :: Expr 'Core
const_ = Lam (Var "x") (Lam (Var "y") (Var "x"))

evalTests :: TestTree
evalTests =
  testGroup
    "Evaluation"
    [ testCase "id 1" $ do
        let e = evalCore env $ Var "id" `App` Int 1
        getValue e @=? Just (1 :: Integer),
      testCase "const 1 2" $ do
        let e = evalCore env $ Var "const" `App` Int 1 `App` Int 2
        getValue e @=? Just (1 :: Integer),
      testCase "id id 1" $ do
        let e = evalCore env $ Var "id" `App` Var "id" `App` Int 1
        getValue e @=? Just (1 :: Integer),
      testCase "id (id 1)" $ do
        let e = evalCore env $ Var "id" `App` (Var "id" `App` Int 1)
        getValue e @=? Just (1 :: Integer),
      testCase "id id id 1" $ do
        let e = evalCore env $ Var "id" `App` Var "id" `App` Var "id" `App` Int 1
        getValue e @=? Just (1 :: Integer),
      testCase "id (const 1) 2" $ do
        let e = evalCore env $ Var "id" `App` (Var "const" `App` Int 1) `App` Int 2
        getValue e @=? Just (1 :: Integer),
      testCase "let x = 1 in x" $ do
        let e = evalCore env $ Let (Var "x") (Int 1) (Var "x")
        getValue e @=? Just (1 :: Integer),
      testCase "let x = 1 in let x = 2 in x" $ do
        let e = evalCore env $ Let (Var "x") (Int 1) (Let (Var "x") (Int 2) (Var "x"))
        getValue e @=? Just (2 :: Integer),
      testCase "case #1 2" $ do
        let e = evalCore env $ Case (Cons 1 (Int 2)) equations
        getValue e @=? Just (2 :: Integer),
      testCase "case #0" $ do
        let e = evalCore env $ Case (Cons 0 (Tuple [])) equations
        getValue e @=? Just (0 :: Integer),
      testCase "tuple projection 1" $ do
        let e = evalCore env $ At (Tuple [String "hello", String "world"]) (TupleIx 1)
        getValue e @=? Just ("hello" :: Text),
      testCase "tuple projection 2" $ do
        let e = evalCore env $ At (Tuple [String "hello", String "world"]) (TupleIx 2)
        getValue e @=? Just ("world" :: Text)
    ]

equations :: Vector (Expr 'Core)
equations =
  [ Lam (Var "x") (Int 0),
    Lam (Var "x") (Var "x")
  ]
