module FreshTest (test) where

import qualified Test.Tasty as Tasty

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocols.Fresh"
    []
