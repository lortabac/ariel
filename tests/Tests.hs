module Main where

import Ariel.Tests.Parse
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ parseIdentifierTests,
      parseLambdaTests,
      parseLetTests,
      parseTupleTests,
      parseCoreCon
    ]
