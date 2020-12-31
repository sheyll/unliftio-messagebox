module Main (main) where

import qualified BoundedMessageBoxTest
import qualified CommandTest
import qualified CornerCaseTests
import qualified FreshTest
import qualified MessageBoxClassTest
import qualified ProtocolsTest
import qualified Test.Tasty as Tasty
import qualified UnboundedMessageBoxTest

main :: IO ()
main = Tasty.defaultMain test

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Tests"
    [ CornerCaseTests.test,
      CommandTest.test,
      MessageBoxClassTest.test,
      BoundedMessageBoxTest.test,
      UnboundedMessageBoxTest.test,
      ProtocolsTest.test,
      FreshTest.test
    ]
