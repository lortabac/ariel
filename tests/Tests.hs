module Main where

import Ariel.Tests.Eval
import Ariel.Tests.IO
import Ariel.Tests.Value
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [evalTests, getValueTests, ioTests]
