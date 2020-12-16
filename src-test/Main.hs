module Main(main) where

import qualified Test.Tasty as Tasty
import qualified GoodConcurrencyTest
import qualified MessageBoxClassTest
import qualified BoundedMessageBoxTest
import qualified UnboundedMessageBoxTest
import qualified ProtocolsTest
import qualified FreshTest

main = Tasty.defaultMain test

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Tests"
    [ MessageBoxClassTest.test,
      BoundedMessageBoxTest.test,
      UnboundedMessageBoxTest.test,
      ProtocolsTest.test,
      FreshTest.test,
      GoodConcurrencyTest.test
    ]
