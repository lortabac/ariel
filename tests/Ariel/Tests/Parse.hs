{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Parse
  ( parseLambdaTests,
  )
where

import Ariel
import Ariel.Syntax.Parse
import Test.Tasty
import Test.Tasty.HUnit

parseLambdaTests :: TestTree
parseLambdaTests =
  testGroup
    "Lambda parsing"
    [ testCase "id" $
        let e = runParseExpr "(x) => x"
         in e @=? Right ("x" ==> Var "x"),
      testCase "id spaces" $
        let e = runParseExpr " (x)  =>  x"
         in e @=? Right ("x" ==> Var "x"),
      testCase "id no spaces" $
        let e = runParseExpr "(x)=>x"
         in e @=? Right ("x" ==> Var "x"),
      testCase "id no-parens arg" $
        let e = runParseExpr "x => x"
         in e @=? Right ("x" ==> Var "x"),
      testCase "id no-parens arg spaces" $
        let e = runParseExpr " x  =>  x"
         in e @=? Right ("x" ==> Var "x"),
      testCase "id parens" $
        let e = runParseExpr "(x => x)"
         in e @=? Right ("x" ==> Var "x"),
      testCase "const" $
        let e = runParseExpr "(x, y) => x"
         in e @=? Right ("x" ==> "y" ==> Var "x"),
      testCase "const curried" $
        let e = runParseExpr "x => y => x"
         in e @=? Right ("x" ==> "y" ==> Var "x"),
      testCase "lambda application id" $
        let e = runParseExpr "(x => x)(1)"
         in e @=? Right (("x" ==> Var "x") @@ Int 1),
      testCase "lambda application const" $
        let e = runParseExpr "((x, y) => x)(1, 2)"
         in e @=? Right (("x" ==> "y" ==> Var "x") @@ Int 1 @@ Int 2),
      testCase "lambda application const curried app" $
        let e = runParseExpr "(((x, y) => x)(1))(2)"
         in e @=? Right (("x" ==> "y" ==> Var "x") @@ Int 1 @@ Int 2)
    ]
