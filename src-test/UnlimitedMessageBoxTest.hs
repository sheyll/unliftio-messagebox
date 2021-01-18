{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module UnlimitedMessageBoxTest (test) where

import MessageBoxCommon (testContentionRobustness)
import qualified UnliftIO.MessageBox.Class as Class
import qualified UnliftIO.MessageBox.Unlimited as Unlimited
import Test.Tasty as Tasty (TestTree, testGroup)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "UnliftIO.MessageBox.Unlimited"
    [ testContentionRobustness (Class.newMessageBox Unlimited.BlockingUnlimited) 512 50
    ]
