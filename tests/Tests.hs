module Main where

import Ariel.Tests.Eval
import Ariel.Tests.Parse
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ evalLambdaTests,
      evalTupleTests,
      evalLetTests,
      evalCaseTests,
      evalRecursionTests,
      parseIdentifierTests,
      parseLambdaTests,
      parseLetTests,
      parseTupleTests
    ]
