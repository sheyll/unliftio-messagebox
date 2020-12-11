{-# LANGUAGE StrictData #-}

module Main where

import qualified Test.Tasty as Tasty
import qualified GoodConcurrencyTests
import qualified MessageBoxTests
import qualified ProtocolsTests
import qualified FreshTests

main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Tests"
    [ MessageBoxTests.tests,
      ProtocolsTests.tests,
      FreshTests.tests,
      GoodConcurrencyTests.tests
    ]
