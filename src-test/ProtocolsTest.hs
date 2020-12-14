module ProtocolsTest(test) where

import qualified Test.Tasty as Tasty

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocols"
    []

{- 

serverProcessDiesBeforeClientCall = error "TODO"

serverProcessDiesDuringClientCall = error "TODO"

serverProcessDiesDuringClientCallNoMemLeak = error "TODO"

clientProcessDiesWhileServerProcessesTheRequest = error "TODO"
-}