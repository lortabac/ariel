{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Ariel.Tests.IO
  ( ioTests,
  )
where

import Ariel
import Test.Tasty
import Test.Tasty.HUnit

ioTests :: TestTree
ioTests =
  testGroup
    "IO"
    [ testCase "pure" $ do
        _ <- runCore [] $ Pure (String "hello")
        assertBool "pure" True,
      testCase "bind" $ do
        _ <- runCore [] $ Pure (String "hello") `Bind` Lam (Var "x") (Pure (Var "x"))
        assertBool "bind" True
    ]
