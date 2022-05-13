{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Eval
  ( evalLambdaTests,
  )
where

import Ariel.Core.Eval
import Ariel.Core.Types
import Test.Tasty
import Test.Tasty.HUnit

evalLambdaTests :: TestTree
evalLambdaTests =
  testGroup
    "Lambda evaluation"
    [ testCase "lambda id" $ do
        let e = evalCore mempty $ ("x" ==> Var "x") @@ Int 1
        e @=? Int 1,
      testCase "lambda const" $ do
        let e = evalCore mempty $ ("x" ==> "_" ==> Var "x") @@ Int 1 @@ Int 2
        e @=? Int 1,
      testCase "lambda shadowing" $ do
        let e = evalCore mempty $ ("x" ==> "x" ==> Var "x") @@ Int 1 @@ Int 2
        e @=? Int 2
    ]
