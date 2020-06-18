{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Parse
  ( parseIdentifierTests,
    parseLambdaTests,
    parseLetTests,
    parseTupleTests,
  )
where

import Ariel
import Ariel.Syntax.Parse
import Data.Either (isLeft)
import Test.Tasty
import Test.Tasty.HUnit

parseIdentifierTests :: TestTree
parseIdentifierTests =
  testGroup
    "Identifier parsing"
    [ testCase "literal identifier" $
        let e = runParseExpr "foo"
         in e @=? Right (Var "foo"),
      testCase "quoted identifier" $
        let e = runParseExpr "'foo'"
         in e @=? Right (Var "foo"),
      testCase "quoted identifier escape" $
        let e = runParseExpr "'foo\\''"
         in e @=? Right (Var "foo'")
    ]

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
         in e @=? Right (("x" ==> "y" ==> Var "x") @@ Int 1 @@ Int 2),
      testCase "lambda application const curried app no parens" $
        let e = runParseExpr "((x, y) => x)(1)(2)"
         in e @=? Right (("x" ==> "y" ==> Var "x") @@ Int 1 @@ Int 2)
    ]

parseLetTests :: TestTree
parseLetTests =
  testGroup
    "let parsing"
    [ testCase "let" $
        let e = runParseExpr "let x = 1, x"
         in e @=? Right (Let "x" (Int 1) (Var "x")),
      testCase "let no space in body" $
        let e = runParseExpr "let x=1,x"
         in e @=? Right (Let "x" (Int 1) (Var "x")),
      testCase "let no space after keyword" $
        let e = runParseExpr "letx = 1, x"
         in assertBool "isLeft" (isLeft e),
      testCase "let nested" $
        let e = runParseExpr "let x = 1, let y = 2, {x, y}"
         in e @=? Right (Let "x" (Int 1) (Let "y" (Int 2) (Tuple [Var "x", Var "y"]))),
      testCase "let nested sub" $
        let e = runParseExpr "let x = let y = 2, y, x"
         in e @=? Right (Let "x" (Let "y" (Int 2) (Var "y")) (Var "x"))
    ]

parseTupleTests :: TestTree
parseTupleTests =
  testGroup
    "tuple parsing"
    [ testCase "unit" $
        let e = runParseExpr "{}"
         in e @=? Right (Tuple []),
      testCase "1-element tuple" $
        let e = runParseExpr "{1}"
         in e @=? Right (Tuple [Int 1]),
      testCase "pair" $
        let e = runParseExpr "{1, \"hello\"}"
         in e @=? Right (Tuple [Int 1, Text "hello"]),
      testCase "nested tuples" $
        let e = runParseExpr "{{1}}"
         in e @=? Right (Tuple [Tuple [Int 1]]),
      testCase "let in tuple 1" $
        let e = runParseExpr "{let x = 1, x, 2}"
         in e @=? Right (Tuple [Let "x" (Int 1) (Var "x"), Int 2]),
      testCase "let in tuple 2" $
        let e = runParseExpr "{2, let x = 1, x, 3}"
         in e @=? Right (Tuple [Int 2, Let "x" (Int 1) (Var "x"), Int 3]),
      testCase "let in tuple parens" $
        let e = runParseExpr "{2, (let x = 1, x), 3}"
         in e @=? Right (Tuple [Int 2, Let "x" (Int 1) (Var "x"), Int 3])
    ]
