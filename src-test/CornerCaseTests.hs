{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test race conditions
module CornerCaseTests (test) where

import Control.Exception
  ( BlockedIndefinitelyOnMVar (BlockedIndefinitelyOnMVar),
  )
import Data.Semigroup (Any (Any, getAny), Semigroup (stimes))
import qualified Protocol.LimitedMessageBox as B
import Protocol.Command
  ( CallId (MkCallId),
    Command,
    CommandError (BlockingCommandTimedOut),
    Message (Blocking),
    ReturnType (Return),
    call,
    newCallIdCounter,
  )
import Protocol.MessageBoxClass
  ( IsInBox (newOutBox2, receive),
    IsInBoxConfig (..),
    IsOutBox (deliver),
    OutBox2,
    handleMessage,
  )
import qualified Protocol.UnlimitedMessageBox as U
import RIO
  ( HasCallStack,
    runRIO,
    threadDelay,
  )
import System.Mem (performMajorGC, performMinorGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import UnliftIO
  ( MVar,
    MonadIO (..),
    MonadUnliftIO,
    concurrently,
    newEmptyMVar,
    putMVar,
    takeMVar,
    timeout,
    try,
  )
import UnliftIO.Concurrent (forkIO, yield)

test :: TestTree
test =
  testGroup
    "CornerCaseTests"
    [ testGroup
        "waiting for messages from a dead process"
        [ testCase "When using the Unlimited Message Box, an exception is thrown" $
            try @_ @BlockedIndefinitelyOnMVar
              (waitForMessageFromDeadProcess U.UnlimitedMessageBox)
              >>= either
                ( assertEqual
                    "wrong exception thrown: "
                    (show BlockedIndefinitelyOnMVar)
                    . show
                )
                (const (assertFailure "Exception expected!")),
          testCase "When using the Limited Message Box, the test will timeout" $
            waitForMessageFromDeadProcess (B.LimitedMessageBox 16)
              >>= assertEqual "unexpected return value: " SecondReceiveTimedOut
        ],
      testGroup
        "sending messages to a dead process"
        [ testCase "When using the Unlimited Message Box, sending messages succeeds" $
            sendMessagesToDeadProcess U.UnlimitedMessageBox
              >>= assertEqual "unexpected result: " SomeMoreMessagesSent,
          testCase "When using the Blocking Limited Message Box, sending messages eventually blocks and times out" $
            sendMessagesToDeadProcess (B.LimitedMessageBox 16)
              >>= assertEqual "unexpected result: " SendingMoreMessagesTimedOut
        ],
      testGroup
        "Command"
        [ testGroup
            "waiting for a call reply after the server died"
            [ testCase "When using the Unlimited Message Box, BlockingCommandTimedOut is returned" $
                waitForCallReplyFromDeadServer U.UnlimitedMessageBox
                  >>= assertEqual
                    "unexpected result: "
                    (CallFailed (BlockingCommandTimedOut (MkCallId 1))),
              testCase "When using the Limited Message Box, BlockingCommandTimedOut is returned" $
                waitForCallReplyFromDeadServer (B.LimitedMessageBox 16)
                  >>= assertEqual
                    "unexpected result: "
                    (CallFailed (BlockingCommandTimedOut (MkCallId 1)))
            ]
        ]
    ]

data WaitForMessageFromDeadProcessResult
  = SecondReceiveReturnedNothing
  | SecondReceiveTimedOut
  | SecondReceiveUnexpectedSuccess
  deriving stock (Show, Eq)

waitForMessageFromDeadProcess ::
  (HasCallStack, IsInBoxConfig cfg inbox) =>
  cfg ->
  IO WaitForMessageFromDeadProcessResult
waitForMessageFromDeadProcess inboxCfg =
  do
    firstMessageSent <- newEmptyMVar
    let msg1 = 42
    inbox <- newInBox inboxCfg
    _ <- forkIO $ do
      outbox <- newOutBox2 inbox
      deliver outbox msg1 >>= assertBool "first message not sent!"
      putMVar firstMessageSent ()
    takeMVar firstMessageSent
    receive inbox
      >>= maybe
        (assertFailure "failed to receive first message")
        (assertEqual "invalid first message received!" msg1)
    threadDelay 10_000
    liftIO performMinorGC
    liftIO performMajorGC
    threadDelay 10_000
    timeout
      200_000
      ( receive inbox
          >>= maybe
            (return SecondReceiveReturnedNothing)
            ( \r -> do
                liftIO (assertEqual "unexpected second message received!" msg1 r)
                return SecondReceiveUnexpectedSuccess
            )
      )
      >>= maybe
        (return SecondReceiveTimedOut)
        return

data SendMessageToDeadProcessResult
  = NoMoreMessagesSent
  | SomeMoreMessagesSent
  | SendingMoreMessagesTimedOut
  deriving stock (Show, Eq)

sendMessagesToDeadProcess ::
  (HasCallStack, IsInBoxConfig cfg inbox) =>
  cfg ->
  IO SendMessageToDeadProcessResult
sendMessagesToDeadProcess inboxCfg =
  do
    ready <- newEmptyMVar
    firstMessageSent <- newEmptyMVar
    done <- newEmptyMVar
    let msg1 = 42

    _receiver <- forkIO $ do
      inbox <- newInBox inboxCfg
      outbox <- newOutBox2 inbox
      putMVar ready outbox
      takeMVar firstMessageSent
      receive inbox >>= putMVar done

    outbox <- takeMVar ready
    deliver outbox msg1 >>= assertBool "first message not sent!"
    putMVar firstMessageSent ()
    takeMVar done >>= assertEqual "first message invalid" (Just msg1)
    threadDelay 10_000
    liftIO performMinorGC
    liftIO performMajorGC
    threadDelay 10_000

    timeout
      1_000_000
      (stimes 100 (Any <$> deliver outbox msg1))
      >>= maybe
        (return SendingMoreMessagesTimedOut)
        ( \r ->
            return $
              if getAny r
                then SomeMoreMessagesSent
                else NoMoreMessagesSent
        )

-- ------------------------------------------------------------

data WaitForCallReplyFromDeadServerResult
  = WaitForCallReplyFromDeadServerResult
  | CallFailed CommandError
  | WaitForCallReplyFromDeadServerNoResult
  deriving stock (Show, Eq)

waitForCallReplyFromDeadServer ::
  (IsInBoxConfig cfg inbox) =>
  cfg ->
  IO WaitForCallReplyFromDeadServerResult
waitForCallReplyFromDeadServer inboxCfg =
  do
    ready <- newEmptyMVar
    (cr, sr) <-
      concurrently
        (waitForCallReplyFromDeadServerClient ready)
        (waitForCallReplyFromDeadServerServer inboxCfg ready)
    assertEqual "unexpected server process exit: " () sr
    return cr

waitForCallReplyFromDeadServerClient ::
  (MonadIO m, IsOutBox o) =>
  MVar (o (Message TestServer)) ->
  m WaitForCallReplyFromDeadServerResult
waitForCallReplyFromDeadServerClient ready = do
  outbox <- takeMVar ready
  cic <- newCallIdCounter
  runRIO cic (threadDelay 1_000 >> call outbox TestCall 100_000)
    >>= either
      (return . CallFailed)
      (const (return WaitForCallReplyFromDeadServerResult))

waitForCallReplyFromDeadServerServer ::
  (IsInBoxConfig cfg inbox) =>
  cfg ->
  MVar (OutBox2 inbox (Message TestServer)) ->
  IO ()
waitForCallReplyFromDeadServerServer inboxCfg ready = do
  inbox <- newInBox inboxCfg
  outbox <- newOutBox2 inbox
  putMVar ready outbox
  -- threadDelay 1_000
  handleLoop inbox
  where
    handleLoop ::
      (MonadUnliftIO m, IsInBox inbox) =>
      inbox (Message TestServer) ->
      m ()
    handleLoop inbox =
      handleMessage
        inbox
        ( \case
            Blocking TestCall _r -> do
              _ <- forkIO $ do
                threadDelay 10_000
                liftIO performMinorGC
                liftIO performMajorGC
              return ()
        )
        >>= maybe
          (yield >> handleLoop inbox)
          return

data TestServer

data instance Command TestServer _ where
  TestCall :: Command TestServer ( 'Return ())

deriving stock instance Show (Command TestServer ( 'Return ()))

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

-- data Counter

-- data instance Command Counter t where
--   Incr :: Command Counter FireAndForget
--   Set :: Int -> Command Counter FireAndForget
--   Get :: Command Counter (Return Int)
