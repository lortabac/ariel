{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Eval
  ( evalLambdaTests,
    evalTupleTests,
    evalLetTests,
    evalCaseTests,
    evalRecursionTests,
  )
where

import Ariel
import qualified Ariel.Evaluation.Types as NL
import Test.Tasty
import Test.Tasty.HUnit

evalLambdaTests :: TestTree
evalLambdaTests =
  testGroup
    "Lambda evaluation"
    [ testCase "lambda id" $ do
        e <- evalNamed $ ("x" ==> Var "x") @@ Int 1
        e @=? NL.Int 1,
      testCase "lambda const" $ do
        e <- evalNamed $ ("x" ==> "_" ==> Var "x") @@ Int 1 @@ Int 2
        e @=? NL.Int 1,
      testCase "lambda shadowing" $ do
        e <- evalNamed $ ("x" ==> "x" ==> Var "x") @@ Int 1 @@ Int 2
        e @=? NL.Int 2
    ]

evalTupleTests :: TestTree
evalTupleTests =
  testGroup
    "Tuple evaluation"
    [ testCase "t.1" $ do
        e <- evalNamed $ GetT 1 (Tuple [Int 1, Int 2])
        e @=? NL.Int 1,
      testCase "(eval t).1" $ do
        e <- evalNamed $ GetT 1 (Tuple [("x" ==> Var "x") @@ Int 1, Int 2])
        e @=? NL.Int 1,
      testCase "update" $ do
        e <- evalNamed $ UpdateT 1 ("n" ==> Prim "Plus" [Var "n", Int 1]) (Tuple [Int 0, Text "hello"])
        e @=? NL.Tuple [NL.Int 1, NL.Text "hello"]
    ]

evalLetTests :: TestTree
evalLetTests =
  testGroup
    "Let evaluation"
    [ testCase "id 1" $ do
        e <- evalNamed $ ("id", "x" ==> Var "x") `in_` Var "id" @@ Int 1
        e @=? NL.Int 1,
      testCase "id id 1" $ do
        e <- evalNamed $ ("id", "x" ==> Var "x") `in_` Var "id" @@ Var "id" @@ Int 1
        e @=? NL.Int 1,
      testCase "id (id 1)" $ do
        e <- evalNamed $ ("id", "x" ==> Var "x") `in_` Var "id" @@ (Var "id" @@ Int 1)
        e @=? NL.Int 1,
      testCase "const 1" $ do
        e <- evalNamed $ ("const", "x" ==> "_" ==> Var "x") `in_` Var "const" @@ Int 1 @@ Int 2
        e @=? NL.Int 1,
      testCase "id const 1" $ do
        e <-
          evalNamed $
            ("id", "x" ==> Var "x")
              `in_` ("const", "x" ==> "_" ==> Var "x")
              `in_` Var "id" @@ Var "const" @@ Int 1 @@ Int 2
        e @=? NL.Int 1,
      testCase "id (const 1)" $ do
        e <-
          evalNamed $
            ("id", "x" ==> Var "x")
              `in_` ("const", "x" ==> "_" ==> Var "x")
              `in_` Var "id" @@ (Var "const" @@ Int 1 @@ Int 2)
        e @=? NL.Int 1,
      testCase "let shadowing" $ do
        e <-
          evalNamed $
            ("one", Text "1")
              `in_` ("one", Int 1)
              `in_` Var "one"
        e @=? NL.Int 1
    ]

evalCaseTests :: TestTree
evalCaseTests =
  testGroup
    "Cons evaluation"
    [ testCase "bool false" $ do
        e <- evalNamed $ CoreCase false [Int 0, Int 1]
        e @=? NL.Int 0,
      testCase "bool true" $ do
        e <- evalNamed $ CoreCase true [Int 0, Int 1]
        e @=? NL.Int 1,
      testCase "just 1" $ do
        e <- evalNamed $ CoreCase (just (Int 1)) [Int 0, "x" ==> Var "x"]
        e @=? NL.Int 1,
      testCase "nothing" $ do
        e <- evalNamed $ CoreCase nothing [Int 0, "x" ==> Var "x"]
        e @=? NL.Int 0,
      testCase "multiarg 0" $ do
        e <- evalNamed $ CoreCase (CoreCons 0 [Int 1, Int 1]) ["x" ==> "y" ==> Prim "Plus" [Var "x", Var "y"]]
        e @=? NL.Int 2,
      testCase "multiarg 1" $ do
        e <- evalNamed $ CoreCase (CoreCons 1 [Int 1, Int 1]) [Int 0, "x" ==> "y" ==> Prim "Plus" [Var "x", Var "y"]]
        e @=? NL.Int 2,
      testCase "var arg" $ do
        e <- evalNamed $ ("one", Int 1) `in_` CoreCase (just (Var "one")) [Int 0, "x" ==> Var "x"]
        e @=? NL.Int 1
    ]

evalRecursionTests :: TestTree
evalRecursionTests =
  testGroup
    "Recursion evaluation"
    [ testCase "sum right" $ do
        e <-
          evalNamed $
            ("sum", "n" ==> CoreCase (Prim "Eq" [Var "n", Int 0]) [Prim "Plus" [Var "sum" @@ Prim "Minus" [Var "n", Int 1], Var "n"], Int 0]) `in_` Var "sum" @@ Int 10
        e @=? NL.Int 55,
      testCase "sum right" $ do
        e <-
          evalNamed $
            ("eq", "x" ==> "y" ==> Prim "Eq" [Var "x", Var "y"]) `in_` ("sum", "n" ==> CoreCase (Var "eq" @@ Var "n" @@ Int 0) [Prim "Plus" [Var "sum" @@ Prim "Minus" [Var "n", Int 1], Var "n"], Int 0]) `in_` Var "sum" @@ Int 10
        e @=? NL.Int 55
    ]

nothing :: Expr
nothing = CoreCons 0 []

just :: Expr -> Expr
just x = CoreCons 1 [x]

false :: Expr
false = CoreCons 0 []

true :: Expr
true = CoreCons 1 []
