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
import Test.Tasty as Tasty (TestTree, testGroup, testCase)
import Test.Tasty.QuickCheck as Tasty (testProperty)
import UnliftIO (conc, runConc)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.UnboundedMessageBox"
    [ Tasty.testGroup
        "Non-Blocking IsOutBox/IsInBox instance"
        [ Tasty.testCase "receive from an empty queue" $ do
            i <- MessageBox.createInBox 10
            timeout 1_000_000 (Class.receive $ InBoxNB i)
              >>= assertEqual "receive must not block" (Just (Nothing @Int)),
          Tasty.testCase "send to full queue" $ do
            i <- MessageBox.createInBox 10
            o <- MessageBox.createOutBoxForInbox i
            let sendWhileNotFull = do
                  success <- Class.deliver (OutBoxNB o) 2
                  if not success then sendWhileNotFull else pure ()
            timeout 1_000_000 sendWhileNotFull
              >>= assertBool "deliver must not block" . isJust
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
