module ProtocolsTests where

import UnliftIO
import Protocols.MessageBox
import qualified Test.Tasty as Tasty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Protocols" []
