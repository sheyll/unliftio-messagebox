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
import qualified Protocol.MessageBoxClass as Class
import Protocol.UnboundedMessageBox as MessageBox
  ( InBoxNB (..),
    createInBox,
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
import Test.Tasty.HUnit as Tasty (assertEqual, testCase)
import Test.Tasty.QuickCheck as Tasty (testProperty)
import UnliftIO (conc, runConc, timeout)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.UnboundedMessageBox"
    [ Tasty.testGroup
        "Non-Blocking IsOutBox/IsInBox instance"
        [ Tasty.testCase "receive from an empty queue" $ do
            i <- MessageBox.createInBox
            timeout 1_000_000 (Class.receive $ InBoxNB i)
              >>= assertEqual "receive must not block" (Just (Nothing @Int))
        ],
      testProperty "all n messages of all k outBoxes are received by the inbox" $
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
