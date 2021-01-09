{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module UnlimitedMessageBoxTest (test) where

import Control.Monad (forM, replicateM)
import Data.Foldable (Foldable (fold))
import Data.Functor (($>))
import Data.Maybe ()
import Data.Monoid (All (All, getAll))
import qualified Protocol.MessageBox.Class as Class
import Protocol.MessageBox.Unlimited as MessageBox
  ( create,
    newInput,
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
import Protocol.Future

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.MessageBox.Unlimited"
    [ Tasty.testGroup
        "Non-Blocking"
        [ Tasty.testCase "receive from an empty queue" $ do
            i <- MessageBox.create
            f <- Class.tryReceive i
            timeout 1_000_000 (tryNow f)
              >>= assertEqual "the future must not block and should return be emtpy" (Just (Nothing @Int))
        ],
      testProperty "all n messages of all k outBoxes are received by the output" $
        \(Positive (Small n)) (Positive (Small k)) ->
          ioProperty $
            getAll . fold <$> do
              output <- MessageBox.create
              runConc
                ( (<>)
                    <$> foldMap
                      ( \i ->
                          conc
                            ( do
                                input <- MessageBox.newInput output
                                forM [0 .. n - 1] (\j -> deliver input ("test message: ", i, j) $> All True)
                            )
                      )
                      [0 .. k - 1]
                    <*> conc (replicateM (n * k) (receive output $> All True))
                )
    ]
