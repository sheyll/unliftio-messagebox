{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances of QuickCheck Type Classes
module QCOrphans () where

import Protocol.MessageBox.CatchAll
import qualified Protocol.MessageBox.Limited as Limited
import Test.QuickCheck

instance Arbitrary Limited.MessageLimit where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Limited.BlockingBoxLimit where
  arbitrary = Limited.BlockingBoxLimit <$> arbitrary

instance Arbitrary Limited.NonBlockingBoxLimit where
  arbitrary = Limited.NonBlockingBoxLimit <$> arbitrary

instance Arbitrary Limited.WaitingBoxLimit where
  arbitrary =
    Limited.WaitingBoxLimit <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary a => Arbitrary (CatchAllFactory a) where
  arbitrary = CatchAllFactory <$> arbitrary