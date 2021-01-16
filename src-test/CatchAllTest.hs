{-# LANGUAGE TypeApplications #-}

module CatchAllTest (test) where

import Data.Proxy
import Protocol.MessageBox.CatchAll
import QCOrphans ()
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit

test :: TestTree
test =
  testGroup
    "CatchAll"
    [ testCase
        "Laws"
        ( lawsCheckMany
            [ ( "CatchAllFactory",
                [ eqLaws (Proxy @(CatchAllFactory Int)),
                  ordLaws (Proxy @(CatchAllFactory Int))
                ]
              )
            ]
        )
    ]