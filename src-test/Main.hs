module Main(main) where

import qualified Test.Tasty as Tasty
import qualified GoodConcurrencyTest
import qualified ClosableMessageBoxTest
import qualified SimplerMessageBoxTest
import qualified ProtocolsTest
import qualified FreshTest

main = Tasty.defaultMain test

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Tests"
    [ SimplerMessageBoxTest.test,
      ClosableMessageBoxTest.test,
      ProtocolsTest.test,
      FreshTest.test,
      GoodConcurrencyTest.test
    ]
