module Main where

import Ariel.Tests.Eval
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [evalLambdaTests]
