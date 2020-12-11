module ProtocolsTests(tests) where

import qualified Test.Tasty as Tasty

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