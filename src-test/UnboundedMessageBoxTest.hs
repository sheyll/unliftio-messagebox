{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module UnboundedMessageBoxTest (test) where

import Control.Monad (forM, replicateM)
import Data.Foldable (Foldable (fold))
import Data.Functor (($>))
import Data.Maybe ()
import Data.Monoid (All (All, getAll))
import Protocol.UnboundedMessageBox as MessageBox
  ( createInBox,
    createOutBoxForInbox,
    deliver,
    receive,
  )
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
    "Protocol.UnboundedMessageBox"
    [ testProperty "all n messages of all k outBoxes are received by the inbox" $
        \(Positive (Small n)) (Positive (Small k)) ->
          ioProperty $
            getAll . fold <$> do
              inbox <- MessageBox.createInBox
              runConc
                ( (<>)
                    <$> foldMap
                      ( \i ->
                          conc
                            ( do
                                outbox <- MessageBox.createOutBoxForInbox inbox
                                forM [0 .. n - 1] (\j -> deliver outbox ("test message: ", i, j) $> All True)
                            )
                      )
                      [0 .. k - 1]
                    <*> conc (replicateM (n * k) (receive inbox $> All True))
                )
    ]
