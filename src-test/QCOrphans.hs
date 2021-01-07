{-# OPTIONS_GHC -Wno-orphans #-}
-- | Orphan instances of QuickCheck Type Classes
module QCOrphans () where

import qualified Protocol.MessageBox.Limited as Limited
import Test.QuickCheck

instance Arbitrary Limited.MessageLimit where
  arbitrary = elements [minBound .. maxBound]
