{-# LANGUAGE FlexibleContexts #-}

module MessageBoxClassTest (test) where

import Control.Monad (forM, replicateM)
import Data.Foldable (Foldable (fold))
import Data.Maybe (isJust)
import Data.Monoid (All (All, getAll))
import qualified Protocol.MessageBox.Limited as B
import Protocol.MessageBox.Class (IsMessageBox (..), IsMessageBoxFactory (..), IsInput (..))
import qualified Protocol.MessageBox.Unlimited as U
import Test.QuickCheck
  ( Positive (Positive),
    Small (Small),
    ioProperty,
  )
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as Tasty (testProperty)
import UnliftIO (conc, runConc)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.MessageBox.Class"
    [ -- TODO receive tests
      -- TODO deliver tests
      -- TODO Timeout tests
      -- TODO CatchExceptions tests

      Tasty.testGroup
        "all n messages of all k outBoxes are received by the output"
        [ realWorldTest U.UnlimitedMessageBox,
          realWorldTest (B.LimitedMessageBox 2),
          realWorldTest (B.LimitedMessageBox 16),
          realWorldTest (B.LimitedMessageBox 128)
        ]
    ]

realWorldTest ::
  (IsMessageBoxFactory cfg output, Show cfg) =>
  cfg ->
  TestTree
realWorldTest arg =
  testProperty (show arg) $
    \(Positive (Small n)) (Positive (Small k)) ->
      ioProperty $
        getAll . fold <$> do
          output <- newMessageBox arg
          runConc
            ( (<>)
                <$> foldMap
                  ( \i ->
                      conc
                        ( do
                            input <- newInput output
                            forM [0 .. n - 1] (\j -> All <$> deliver input ("test message: ", i, j))
                        )
                  )
                  [0 .. k - 1]
                <*> conc (replicateM (n * k) (All . isJust <$> receive output))
            )
