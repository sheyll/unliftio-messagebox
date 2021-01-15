module Main (main) where

import qualified LimitedMessageBoxTest
import qualified CallIdTest
import qualified CommandTest
import qualified CornerCaseTests
import qualified FreshTest
import qualified MessageBoxClassTest
import qualified ProtocolsTest
import System.Environment ( setEnv )
import qualified Test.Tasty as Tasty
import qualified UnlimitedMessageBoxTest

main :: IO ()
main =
  do
    setEnv "TASTY_NUM_THREADS" "1"
    Tasty.defaultMain test

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Tests"
    [ CallIdTest.test,
      CornerCaseTests.test,
      CommandTest.test,
      MessageBoxClassTest.test,
      LimitedMessageBoxTest.test,
      UnlimitedMessageBoxTest.test,
      ProtocolsTest.test,
      FreshTest.test
    ]
