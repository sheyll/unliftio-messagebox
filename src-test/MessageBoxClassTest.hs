{-# LANGUAGE FlexibleContexts #-}

module MessageBoxClassTest (test) where

import Control.Monad (forM, replicateM)
import Data.Foldable (Foldable (fold))
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Monoid (All (All, getAll))
import qualified Protocol.BoundedMessageBox as B
import Protocol.MessageBoxClass ( IsMessageBox(..), IsInBox(..), IsOutBox(..) )
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
    [ Tasty.testGroup
        "all n messages of all k outBoxes are received by the inbox"
        [ deliverAllMessages U.UnboundedMessageBox,
          deliverAllMessages (B.BoundedMessageBox 2),
          deliverAllMessages (B.BoundedMessageBox 16),
          deliverAllMessages (B.BoundedMessageBox 128)
        ]
    ]

deliverAllMessages ::
  (IsMessageBox inbox outbox, Show (InBoxConfig inbox),
  IsInBox inbox, IsOutBox outbox) =>
  InBoxConfig inbox ->
  TestTree
deliverAllMessages arg =
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
                            outbox <- newOutBox inbox
                            forM [0 .. n - 1] (\j -> All <$> deliver outbox ("test message: ", i, j))
                        )
                  )
                  [0 .. k - 1]
                <*> conc (replicateM (n * k) (receive inbox >>= return . All . isJust))
            )
