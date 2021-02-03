-- | Fast and robust message queues for concurrent processes.
--
-- Processes of an application
-- can exchange message using __Message Boxes__.
--
-- This library is meant to be a wrapper around a
-- well tested and benchmarked subset of @unagi-chan@
-- for applications using @unliftio@.
--
-- In addition to the basic functionality, i.e.
-- _Message Boxes_, there is a very little bit of
-- type level magic dust in "UnliftIO.MessageBox.Command"
-- that helps to write code that sends a message and
-- expects the receiving process to send a reply.
--
-- This module re-exports most of the library.
module UnliftIO.MessageBox
  ( module UnliftIO.MessageBox.Class,
    module UnliftIO.MessageBox.Limited,
    module UnliftIO.MessageBox.Unlimited,
    module UnliftIO.MessageBox.CatchAll,
    module UnliftIO.MessageBox.Command,
    module UnliftIO.MessageBox.Util.CallId,
    module UnliftIO.MessageBox.Util.Fresh,
    module UnliftIO.MessageBox.Util.Future,
  )
where

import UnliftIO.MessageBox.CatchAll
  ( CatchAllBox (..),
    CatchAllArg (..),
    CatchAllInput (..),
  )
import UnliftIO.MessageBox.Class
  ( IsInput (..),
    IsMessageBox (..),
    IsMessageBoxArg (..),
    handleMessage,
  )
import UnliftIO.MessageBox.Command
  ( AsyncReply,
    Command,
    CommandError (..),
    DuplicateReply (..),
    Message (..),
    ReplyBox,
    ReturnType (..),
    call,
    callAsync,
    cast,
    delegateCall,
    replyTo,
    tryTakeReply,
    waitForReply,
  )
import UnliftIO.MessageBox.Limited
  ( BlockingBox (),
    BlockingBoxLimit (..),
    BlockingInput (),
    MessageLimit (..),
    NonBlockingBox (),
    NonBlockingBoxLimit (..),
    NonBlockingInput (..),
    WaitingBox (..),
    WaitingBoxLimit (..),
    WaitingInput (..),
    messageLimitToInt,
  )
import UnliftIO.MessageBox.Unlimited
  ( BlockingUnlimited (..),
    UnlimitedBox,
    UnlimitedBoxInput,
  )
import UnliftIO.MessageBox.Util.CallId
  ( CallId (..),
    HasCallIdCounter (..),
    newCallIdCounter,
    takeNext,
  )
import UnliftIO.MessageBox.Util.Fresh
  ( CounterVar,
    HasCounterVar (..),
    fresh,
    incrementAndGet,
    newCounterVar,
  )
import UnliftIO.MessageBox.Util.Future
  ( Future (..),
    awaitFuture,
    tryNow,
  )
