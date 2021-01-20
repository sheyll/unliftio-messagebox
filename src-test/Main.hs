module Main (main) where

import qualified CallIdTest
import qualified CatchAllTest
import qualified CommandTest
import qualified CornerCaseTests
import qualified FreshTest
import qualified LimitedMessageBoxTest
import qualified MessageBoxClassTest
import System.Environment (setEnv)
import Test.Tasty
  ( TestTree,
    defaultIngredients,
    defaultMainWithIngredients,
    testGroup,
  )
import Test.Tasty.Runners.Html (htmlRunner)
import qualified UnlimitedMessageBoxTest

main :: IO ()
main =
  do
    setEnv "TASTY_NUM_THREADS" "1"
    defaultMainWithIngredients (htmlRunner : defaultIngredients) test

test :: TestTree
test =
  testGroup
    "Tests"
    [ CallIdTest.test,
      CatchAllTest.test,
      CornerCaseTests.test,
      CommandTest.test,
      MessageBoxClassTest.test,
      LimitedMessageBoxTest.test,
      UnlimitedMessageBoxTest.test,
      FreshTest.test
    ]
