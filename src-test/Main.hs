module Main (main) where

import qualified BoundedMessageBoxTest
import qualified CommandTest
import qualified CornerCaseTests
import qualified FreshTest
import qualified MessageBoxClassTest
import qualified ProtocolsTest
import System.Environment ( setEnv )
import qualified Test.Tasty as Tasty
import qualified UnboundedMessageBoxTest

main :: IO ()
main =
  do
    setEnv "TASTY_NUM_THREADS" "1"
    Tasty.defaultMain test

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
