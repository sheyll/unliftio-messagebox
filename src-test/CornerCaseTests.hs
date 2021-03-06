-- | Test race conditions
module CornerCaseTests (test) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Semigroup (Any (Any, getAny), Semigroup (stimes))
import GHC.Stack (HasCallStack)
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
  )
import UnliftIO.Concurrent (forkIO, threadDelay, yield)
import UnliftIO.MessageBox.Class
  ( Input,
    IsInput (deliver),
    IsMessageBox (newInput, receive),
    IsMessageBoxArg (..),
    handleMessage,
    receiveAfter,
  )
import UnliftIO.MessageBox.Command
  ( Command,
    CommandError (BlockingCommandTimedOut),
    Message (Blocking),
    ReturnType (Return),
    call,
  )
import qualified UnliftIO.MessageBox.Limited as B
import qualified UnliftIO.MessageBox.Unlimited as U
import UnliftIO.MessageBox.Util.CallId
  ( CallId (MkCallId),
  )
import qualified UnliftIO.MessageBox.Util.CallId as CallId

test :: TestTree
test =
  testGroup
    "CornerCaseTests"
    [ testGroup
        "waiting for messages from a dead process"
        [ testCase "When using the UnlimitedMessageBox, receive will timeout" $
            waitForMessageFromDeadProcess U.BlockingUnlimited
              >>= assertEqual "unexpected return value: " SecondReceiveReturnedNothing,
          testCase "When using the Limited Message BlockingBox, the test will timeout" $
            waitForMessageFromDeadProcess (B.BlockingBoxLimit B.MessageLimit_16)
              >>= assertEqual "unexpected return value: " SecondReceiveReturnedNothing,
          testCase "When using the Limited Waiting Box, the test will timeout" $
            waitForMessageFromDeadProcess (B.WaitingBoxLimit Nothing 1_000_000 B.MessageLimit_16)
              >>= assertEqual "unexpected return value: " SecondReceiveReturnedNothing,
          testCase "When using the Limited NonBlocking Box, the test will timeout" $
            waitForMessageFromDeadProcess (B.NonBlockingBoxLimit B.MessageLimit_16)
              >>= assertEqual "unexpected return value: " SecondReceiveReturnedNothing
        ],
      testGroup
        "sending messages to a dead process"
        [ testCase "When using the UnlimitedMessageBox, sending messages succeeds" $
            sendMessagesToDeadProcess U.BlockingUnlimited
              >>= assertEqual "unexpected result: " SomeMoreMessagesSent,
          testCase "When using the BlockingBoxLimit, sending messages eventually blocks and times out" $
            sendMessagesToDeadProcess (B.BlockingBoxLimit B.MessageLimit_16)
              >>= assertEqual "unexpected result: " SendingMoreMessagesTimedOut,
          testCase "When using the NonBlockingBoxLimit, sending messages eventually returns False" $
            sendMessagesToDeadProcess (B.NonBlockingBoxLimit B.MessageLimit_16)
              >>= assertEqual "unexpected result: " SomeMoreMessagesSent,
          testCase "When using the WaitingBoxLimit Nothing, sending messages eventually blocks and times out" $
            sendMessagesToDeadProcess (B.WaitingBoxLimit Nothing 1_000 B.MessageLimit_16)
              >>= assertEqual "unexpected result: " SomeMoreMessagesSent
        ],
      testGroup
        "Command"
        [ testGroup
            "waiting for a call reply after the server died"
            [ testCase "When using the UnlimitedMessageBox, BlockingCommandTimedOut is returned" $
                waitForCallReplyFromDeadServer U.BlockingUnlimited
                  >>= assertEqual
                    "unexpected result: "
                    (CallFailed (BlockingCommandTimedOut (MkCallId 1))),
              testCase "When using the BlockingBoxLimit, BlockingCommandTimedOut is returned" $
                waitForCallReplyFromDeadServer (B.BlockingBoxLimit B.MessageLimit_16)
                  >>= assertEqual
                    "unexpected result: "
                    (CallFailed (BlockingCommandTimedOut (MkCallId 1))),
              testCase "When using the NonBlockingBoxLimit, BlockingCommandTimedOut is returned" $
                waitForCallReplyFromDeadServer (B.NonBlockingBoxLimit B.MessageLimit_16)
                  >>= assertEqual
                    "unexpected result: "
                    (CallFailed (BlockingCommandTimedOut (MkCallId 1))),
              testCase "When using WaitingBoxLimit Nothing, BlockingCommandTimedOut is returned" $
                waitForCallReplyFromDeadServer (B.WaitingBoxLimit Nothing 123_456 B.MessageLimit_16)
                  >>= assertEqual
                    "unexpected result: "
                    (CallFailed (BlockingCommandTimedOut (MkCallId 1)))
            ]
        ]
    ]

data WaitForMessageFromDeadProcessResult
  = SecondReceiveReturnedNothing
  | SecondReceiveWasSuccessful
  deriving stock (Show, Eq)

waitForMessageFromDeadProcess ::
  (HasCallStack, IsMessageBoxArg cfg) =>
  cfg ->
  IO WaitForMessageFromDeadProcessResult
waitForMessageFromDeadProcess outputCfg =
  do
    firstMessageSent <- newEmptyMVar
    let msg1 :: Int
        msg1 = 42
    output <- newMessageBox outputCfg
    _ <- forkIO $ do
      input <- newInput output
      deliver input msg1 >>= assertBool "first message not sent!"
      putMVar firstMessageSent ()
    takeMVar firstMessageSent
    receive output
      >>= maybe
        (assertFailure "failed to receive first message")
        (assertEqual "invalid first message received!" msg1)
    threadDelay 10_000
    liftIO performMinorGC
    liftIO performMajorGC
    threadDelay 10_000
    receiveAfter output 3_000_000
      >>= maybe
        (return SecondReceiveReturnedNothing)
        (const (return SecondReceiveWasSuccessful))

data SendMessageToDeadProcessResult
  = NoMoreMessagesSent
  | SomeMoreMessagesSent
  | SendingMoreMessagesTimedOut
  deriving stock (Show, Eq)

sendMessagesToDeadProcess ::
  (HasCallStack, IsMessageBoxArg cfg) =>
  cfg ->
  IO SendMessageToDeadProcessResult
sendMessagesToDeadProcess outputCfg =
  do
    ready <- newEmptyMVar
    firstMessageSent <- newEmptyMVar
    done <- newEmptyMVar
    let msg1 :: Int
        msg1 = 42

    _receiver <- forkIO $ do
      output <- newMessageBox outputCfg
      input <- newInput output
      putMVar ready input
      takeMVar firstMessageSent
      receive output >>= putMVar done

    input <- takeMVar ready
    deliver input msg1
      >>= assertBool "first message not sent!"
    putMVar firstMessageSent ()
    takeMVar done
      >>= assertEqual "first message invalid" (Just msg1)
    threadDelay 10_000
    liftIO performMinorGC
    liftIO performMajorGC
    threadDelay 10_000

    timeout
      2_000_000
      (stimes (100 :: Int) (Any <$> deliver input msg1))
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
  (IsMessageBoxArg cfg) =>
  cfg ->
  IO WaitForCallReplyFromDeadServerResult
waitForCallReplyFromDeadServer outputCfg =
  do
    ready <- newEmptyMVar
    (cr, sr) <-
      concurrently
        (waitForCallReplyFromDeadServerClient ready)
        (waitForCallReplyFromDeadServerServer outputCfg ready)
    assertEqual "unexpected server process exit: " () sr
    return cr

waitForCallReplyFromDeadServerClient ::
  (MonadIO m, IsInput o, MonadUnliftIO m) =>
  MVar (o (Message TestServer)) ->
  m WaitForCallReplyFromDeadServerResult
waitForCallReplyFromDeadServerClient ready = do
  input <- takeMVar ready
  cic <- CallId.newCallIdCounter
  runReaderT (threadDelay 1_000 >> call input TestCall 100_000) cic
    >>= either
      (return . CallFailed)
      (const (return WaitForCallReplyFromDeadServerResult))

waitForCallReplyFromDeadServerServer ::
  (IsMessageBoxArg cfg) =>
  cfg ->
  MVar (Input (MessageBox cfg) (Message TestServer)) ->
  IO ()
waitForCallReplyFromDeadServerServer outputCfg ready = do
  output <- newMessageBox outputCfg
  input <- newInput output
  putMVar ready input
  -- threadDelay 1_000
  handleLoop output
  where
    handleLoop ::
      (MonadUnliftIO m, IsMessageBox output) =>
      output (Message TestServer) ->
      m ()
    handleLoop output =
      handleMessage
        output
        ( \case
            Blocking TestCall _r -> do
              _ <- forkIO $ do
                threadDelay 10_000
                liftIO performMinorGC
                liftIO performMajorGC
              return ()
        )
        >>= maybe
          (yield >> handleLoop output)
          return

data TestServer

data instance Command TestServer _ where
  TestCall :: Command TestServer ('Return ())

deriving stock instance Show (Command TestServer ('Return ()))

-- Simple server loop

-- data ServerState protocol model = MkServerState
--   { state :: model,
--     self :: Input protocol
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
--   m (Either initError (Input protocol))
-- forkServer = undefined

-- data Counter

-- data instance Command Counter t where
--   Incr :: Command Counter FireAndForget
--   Set :: Int -> Command Counter FireAndForget
--   Get :: Command Counter (Return Int)
