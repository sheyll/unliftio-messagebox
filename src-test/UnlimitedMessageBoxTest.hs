{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module UnlimitedMessageBoxTest (test) where

import MessageBoxCommon (testContentionRobustness)
import qualified Protocol.MessageBox.Class as Class
import qualified Protocol.MessageBox.Unlimited as Unlimited
import Test.Tasty as Tasty (TestTree, testGroup)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.MessageBox.Unlimited"
    [ testContentionRobustness (Class.newMessageBox Unlimited.UnlimitedMessageBox) 512 50
    ]
