{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances of QuickCheck Type Classes
module QCOrphans () where

import UnliftIO.MessageBox.Util.CallId
import UnliftIO.MessageBox.CatchAll (CatchAllArg (..))
import qualified UnliftIO.MessageBox.Limited as Limited
import Test.QuickCheck (Arbitrary (arbitrary), elements)

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

instance Arbitrary a => Arbitrary (CatchAllArg a) where
  arbitrary = CatchAllArg <$> arbitrary

instance Arbitrary CallId where
  arbitrary = MkCallId <$> arbitrary