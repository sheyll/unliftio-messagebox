{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
module GoodConcurrencyTests (tests) where

import Protocol.Command
    ( ReturnType(Return, FireAndForget), Command )
import qualified Test.Tasty as Tasty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Integration.GoodConcurrency"
    []

-- Simple server loop

-- data ServerState protocol model = MkServerState
--   { state :: model,
--     self :: OutBox protocol
--   }

-- data ServerLoopResult model where
--   Continue :: ServerLoopResult model
--   ContinueWith :: model -> ServerLoopResult model
--   StopServerLoop :: ServerLoopResult model

-- type InitCallback protocol model err m =
--   ServerState protocol () -> m (Either err model)

-- type UpdateCallback protocol model m =
--   ServerState protocol model -> Message protocol -> m (ServerLoopResult model)

-- type CleanupCallback protocol model m =
--   ServerState protocol model -> m ()

-- forkServer ::
--   InitCallback protocol model initErrror m ->
--   UpdateCallback protocol model m ->
--   CleanupCallback protocol model m ->
--   m (Either initError (OutBox protocol))
-- forkServer = undefined

data Counter

data instance Command Counter t where
  Incr :: Command Counter FireAndForget
  Set :: Int -> Command Counter FireAndForget
  Get :: Command Counter (Return Int)
