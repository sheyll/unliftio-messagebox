{-# LANGUAGE StrictData #-}

module Test where

import Data.Kind
import Data.Text (Text)
import Data.Word
import Numeric.Natural (Natural)
import System.Mem.Weak (Weak)
import qualified Test.Tasty as Tasty
import UnliftIO
import UnliftIO.STM
import Protocols
import qualified MessageBoxTests
import qualified ProtocolsTests
import qualified UniqueCallIdsTests

main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Tests"
    [ MessageBoxTests.tests,
      ProtocolsTests.tests,
      UniqueCallIdsTests.tests,
      GoodConcurrency.tests
    ]

--




-- Simple server loop

-- todo make unagi bounded queue

data ServerState protocol model = MkServerState
  { state :: model,
    self :: OutBox protocol
  }

data ServerLoopResult model where
  ContinueWith :: model -> ServerLoopResult model
  StopServerLoop :: ServerLoopResult model

type InitCallback protocol model err m =
  ServerState protocol () -> m (Either err model)

type UpdateCallback protocol model m =
  ServerState protocol model -> Message protocol -> m (ServerLoopResult model)

type CleanupCallback protocol model m =
  ServerState protocol model -> m ()

forkServer ::
  InitCallback protocol model initErrror m ->
  UpdateCallback protocol model m ->
  CleanupCallback protocol model m ->
  m (Either initError (OutBox protocol))
forkServer = undefined

cast ::
  (Understands (Pdu someProtocol (Sync result))) =>
  OutBox protocol ->
  Pdu protocol Async ->
  m ()
call ::
  ( Understands (Pdu (Response result) Async) client,
    Understands (Pdu someProtocol (Sync result))
  ) =>
  Pdu protocol (Sync result) =>
  OutBox protocol ->
  protocol ->
  m result

data Counter

data instance Pdu Counter where
  Incr :: Pdu Counter Async
  Set :: Int -> Pdu Counter Async
  Get :: Pdu Counter (Synchronization Int)
