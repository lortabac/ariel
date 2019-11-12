{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Eval
  ( evalTests,
  )
where

import Ariel
import Data.Map (Map)
import Test.Tasty
import Test.Tasty.HUnit

env :: Env (Expr 'Core)
env = [("id", id_), ("const", const_)]

hoasEnv :: Env ExprH
hoasEnv = envToHoas env ["id", "const"]

id_ :: Expr 'Core
id_ = Lam (Var "x") (Var "x")

const_ :: Expr 'Core
const_ = Lam (Var "x") (Lam (Var "y") (Var "x"))

evalTests :: TestTree
evalTests =
  testGroup
    "Evaluation"
    [ testCase "id 1" $ do
        let e = evalCore hoasEnv $ Var "id" `App` Int 1
        getValue e @=? Just (1 :: Integer),
      testCase "const 1 2" $ do
        let e = evalCore hoasEnv $ Var "const" `App` Int 1 `App` Int 2
        getValue e @=? Just (1 :: Integer),
      testCase "id id 1" $ do
        let e = evalCore hoasEnv $ Var "id" `App` Var "id" `App` Int 1
        getValue e @=? Just (1 :: Integer),
      testCase "id (id 1)" $ do
        let e = evalCore hoasEnv $ Var "id" `App` (Var "id" `App` Int 1)
        getValue e @=? Just (1 :: Integer),
      testCase "id id id 1" $ do
        let e = evalCore hoasEnv $ Var "id" `App` Var "id" `App` Var "id" `App` Int 1
        getValue e @=? Just (1 :: Integer),
      testCase "id (const 1) 2" $ do
        let e = evalCore hoasEnv $ Var "id" `App` (Var "const" `App` Int 1) `App` Int 2
        getValue e @=? Just (1 :: Integer),
      testCase "let x = 1 in x" $ do
        let e = evalCore hoasEnv $ Let (Var "x") (Int 1) (Var "x")
        getValue e @=? Just (1 :: Integer),
      testCase "let x = 1 in let x = 2 in x" $ do
        let e = evalCore hoasEnv $ Let (Var "x") (Int 1) (Let (Var "x") (Int 2) (Var "x"))
        getValue e @=? Just (2 :: Integer),
      testCase "case Just 2" $ do
        let e = evalCore hoasEnv $ Case (Cons "Just" (Int 2)) equations
        getValue e @=? Just (2 :: Integer),
      testCase "case Nothing" $ do
        let e = evalCore hoasEnv $ Case (Cons "Nothing" (Tuple [])) equations
        getValue e @=? Just (0 :: Integer)
    ]

equations :: Map Tag (Expr 'Core)
equations =
  [ ("Nothing", Lam (Var "x") (Int 0)),
    ("Just", Lam (Var "x") (Var "x"))
  ]
