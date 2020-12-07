module MessageBoxTests where

import Protocols.MessageBox
import qualified Test.Tasty as Tasty
import UnliftIO

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Protocols.MessageBox"
    []
