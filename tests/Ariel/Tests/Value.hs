{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.Value
  ( getValueTests,
  )
where

import Ariel
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

getValueTests :: TestTree
getValueTests =
  testGroup
    "Value"
    [ testCase "int" $
        getValue (IntH 1) @=? Just (1 :: Integer),
      testCase "string" $
        getValue (StringH "hello") @=? Just ("hello" :: Text),
      testCase "float" $
        getValue (FloatH 3.14) @=? Just (3.14 :: Double),
      testCase "pair" $
        getValue (TupleH [IntH 1, StringH "one"])
          @=? Just (1 :: Integer, "one" :: Text),
      testCase "nested pairs" $
        getValue (TupleH [IntH 1, (TupleH [StringH "one", StringH "two"])])
          @=? Just (1 :: Integer, ("one" :: Text, "two" :: Text))
    ]
