module ProtocolsTests where

import Protocols.MessageBox
import qualified Test.Tasty as Tasty
import UnliftIO

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Protocols"
    []

{- 
serverProcessDiesBeforeClientCall = error "TODO"

serverProcessDiesDuringClientCall = error "TODO"

serverProcessDiesDuringClientCallNoMemLeak = error "TODO"

clientProcessDiesWhileServerProcessesTheRequest = error "TODO"
-}