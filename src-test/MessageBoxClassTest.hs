{-# LANGUAGE FlexibleContexts #-}

module MessageBoxClassTest (test) where

import Control.Monad (forM, replicateM)
import Data.Foldable (Foldable (fold))
import Data.Maybe (isJust)
import Data.Monoid (All (All, getAll))
import qualified Protocol.BoundedMessageBox as B
import Protocol.MessageBoxClass (IsInBox (..), IsInBoxConfig (..), IsOutBox (..))
import qualified Protocol.UnboundedMessageBox as U
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
    "Protocol.MessageBoxClass"
    [ -- TODO receive tests
      -- TODO deliver tests
      -- TODO Timeout tests
      -- TODO CatchExceptions tests

      Tasty.testGroup
        "all n messages of all k outBoxes are received by the inbox"
        [ realWorldTest U.UnboundedMessageBox,
          realWorldTest (B.BoundedMessageBox 2),
          realWorldTest (B.BoundedMessageBox 16),
          realWorldTest (B.BoundedMessageBox 128)
        ]
    ]

realWorldTest ::
  (IsInBoxConfig cfg inbox, Show cfg) =>
  cfg ->
  TestTree
realWorldTest arg =
  testProperty (show arg) $
    \(Positive (Small n)) (Positive (Small k)) ->
      ioProperty $
        getAll . fold <$> do
          inbox <- newInBox arg
          runConc
            ( (<>)
                <$> foldMap
                  ( \i ->
                      conc
                        ( do
                            outbox <- newOutBox2 inbox
                            forM [0 .. n - 1] (\j -> All <$> deliver outbox ("test message: ", i, j))
                        )
                  )
                  [0 .. k - 1]
                <*> conc (replicateM (n * k) (All . isJust <$> receive inbox))
            )
