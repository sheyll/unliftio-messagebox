module CallIdTest (test) where

import Control.Monad (replicateM)
import Data.List (nub)
import Protocol.Command.CallId (CallId (MkCallId))
import qualified Protocol.Command.CallId as CallId
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Large (getLarge),
    NonEmptyList (getNonEmpty),
    Positive (..),
    PrintableString (..),
    Property,
    ioProperty,
    testProperty,
  )
import UnliftIO
  ( takeMVar,
    try,
  )
import UnliftIO.Concurrent (forkIO)
import Utils (withCallIds)

test :: TestTree
test =
  testGroup
    "Protocol.Command.CallId"
    [ testCase
        "Even when a CallId CounterVar is shared by multiple threads/processes, all callIds are unique."
        $ do
          callIds <- withCallIds (replicateM 1000 CallId.takeNext)
          assertEqual
            "all callids must be unique"
            (nub callIds)
            callIds
    ]
