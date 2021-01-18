{-# LANGUAGE TypeApplications #-}

module CallIdTest (test) where

import Control.Monad (replicateM)
import Data.Data
import Data.List (nub)
import qualified UnliftIO.MessageBox.Util.CallId as CallId
import QCOrphans ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    testCase,
  )
import Test.Tasty.QuickCheck (testProperty)
import UnliftIO
  ( replicateConcurrently,
  )
import Utils (allEqOrdShowMethodsImplemented, withCallIds)

test :: TestTree
test =
  testGroup
    "UnliftIO.MessageBox.Util.CallId"
    [ testCase
        "Even when a CallId CounterVar is shared by multiple threads/processes, all callIds are unique"
        $ do
          callIds <- withCallIds (replicateConcurrently 1000 CallId.takeNext)
          assertEqual
            "all callids must be unique"
            (nub callIds)
            callIds,
      testCase
        "The next callId is greater and not equal to the previous"
        $ do
          [c1, c2] <- withCallIds (replicateM 2 CallId.takeNext)
          assertBool
            "next callId is not greater"
            (c2 > c1)
          assertBool
            "next callId is equal to the previous"
            (c2 /= c1),
      testProperty
        "more recent CallIds are > (greater than) all previous CallIds"
        $ \(Positive (Small n)) -> ioProperty $ do
          callIds <- withCallIds (replicateM 1000 CallId.takeNext)
          return $
            n > 1 ==> (head callIds < last callIds),
      -- This ONLY exists because I wanted the test-code coverage to be 100%            
      testCase
        "CallId Show instance"
        (assertEqual "bad show result" "<123>" (show (CallId.MkCallId 123))),
      testProperty "CallId Laws" (allEqOrdShowMethodsImplemented (Proxy @CallId.CallId))
    ]
