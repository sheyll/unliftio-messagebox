module UniqeCallIdsTests where

import UnliftIO
import Protocols.UniqeCallIds
import qualified Test.Tasty as Tasty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Protocols.UniqeCallIds" []
