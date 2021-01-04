{-# LANGUAGE StrictData #-}

-- | Abstractions for the definition of
-- 'Command' 'Messages', that flow between
module Protocol.Command
  ( Message (..),
    Command,
    ReturnType (..),
    ReplyBox (),
    CommandError (..),
    cast,
    call,
    replyTo,
    enqueueCall,
    delegateCall,
    PendingReply (),
    waitForReply,
    tryTakeReply,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import Protocol.Command.CallId
  ( CallId (),
    HasCallIdCounter,
  )
import qualified Protocol.Command.CallId as CallId
import qualified Protocol.MessageBox.Class as MessageBox
import UnliftIO
  ( MonadUnliftIO,
    TMVar,
    atomically,
    checkSTM,
    newEmptyTMVarIO,
    putTMVar,
    readTVar,
    registerDelay,
    takeTMVar,
  )

-- | This family allows to encode imperative /commands/.
--
-- The clauses of a 'Command' define the commands that
-- a process should execute.
--
-- Every clause may specify an individual 'ReturnType' that
-- declares if and what response is valid for a message.
--
-- For example:
--
-- >
-- > type LampId = Int
-- >
-- > data instance Command LightControl r where
-- >   GetLamps :: Command LigthControl (Return [LampId])
-- >   SwitchOn :: LampId -> Command LigthControl FireAndForget
-- >
-- > data LightControl -- the phantom type
-- >
--
-- The type index of the Command family is the uninhabited
-- @LightControl@ type.
-- .
--
-- The second type parameter indicates if a message requires the
-- receiver to send a reply back to the blocked and waiting
-- sender, or if no reply is necessary.
data family Command apiTag :: ReturnType -> Type

-- | Indicates if a 'Command' requires the
-- receiver to send a reply or not.
data ReturnType where
  -- | Indicates that a 'Command' value is sent _one-way_.
  --
  -- Values of a 'Command' instance with 'FireAndForget' as second
  -- parameter indicate that the sender should not expect any direct
  -- answer from the recepient.
  FireAndForget :: ReturnType
  -- | Indicates that a 'Command' value requires the receiver
  -- to send a reply of the given type.
  --
  -- Values of a 'Command' instance with 'Return' as second parameter
  -- are received wrapped into a 'Blocking'.
  Return :: Type -> ReturnType

-- | A message valid for some user defined @apiTag@.
--
-- The @apiTag@ tag (phantom-) type defines the
-- messages allowed here, declared by the instance of
-- 'Command' for 'apiTag'.
data Message apiTag where
  -- | Wraps a 'Command' with a 'ReturnType' of 'Return' @result@.
  --
  -- Such a message can formed by using 'call'.
  --
  -- A 'Blocking' contains a 'ReplyBox' that can be
  -- used to send the reply to the other process
  -- blocking on 'call'
  Blocking ::
    Show (Command apiTag ( 'Return result)) =>
    Command apiTag ( 'Return result) ->
    ReplyBox result ->
    Message apiTag
  -- | If the 'Command' has a 'ReturnType' of 'FireAndForget'
  -- it has fire-and-forget semantics.
  --
  -- The smart constructor 'cast' can be used to
  -- this message.
  NonBlocking ::
    (Show (Command apiTag 'FireAndForget)) =>
    Command apiTag 'FireAndForget ->
    Message apiTag

instance Show (Message apiTag) where
  showsPrec d (NonBlocking !m) =
    showParen (d >= 9) (showString "NonBlocking " . showsPrec 9 m)
  showsPrec d (Blocking !m (MkReplyBox _ !callId)) =
    showParen (d >= 9) (showString "Blocking " . showsPrec 9 m . showChar ' ' . showsPrec 9 callId)

-- | This is like 'Input', it can be used
-- by the receiver of a 'Blocking'
-- to either send a reply using 'reply'
-- or to fail/abort the request using 'sendRequestError'
-- data ReplyBox a = MkReplyBox
--   { _replyBox :: MVar (InternalReply a),
--     _replyBoxCallId :: CallId
--   }
data ReplyBox a = MkReplyBox
  { _replyBox :: TMVar (InternalReply a),
    _replyBoxCallId :: CallId
  }

-- | This is the reply to a 'Blocking' sent through the 'ReplyBox'.
type InternalReply a = (CallId, Either CommandError a)

-- | The failures that the receiver of a 'Return' 'Command', i.e. a 'Blocking',
-- can communicate to the /caller/, in order to indicate that
-- processing a request did not or will not lead to the result the
-- caller is blocked waiting for.
data CommandError where
  -- | Failed to enqueue a 'Blocking' 'Command' 'Message' into the corresponding
  -- 'MessageBox.Input'
  CouldNotEnqueueCommand :: CallId -> CommandError
  -- | The request has failed /for reasons/.
  BlockingCommandFailure :: CallId -> CommandError
  -- | Timeout waiting for the result.
  BlockingCommandTimedOut :: CallId -> CommandError
  deriving stock (Show, Eq)

-- | Enqueue a 'NonBlocking' 'Message' into an 'Input'.
-- This is just for symetry to 'call', this is
-- equivalent to: @\obox -> MessageBox.tryToDeliver obox . NonBlocking@
--
-- The
{-# INLINE cast #-}
cast ::
  ( MonadUnliftIO m,
    MessageBox.IsInput o,
    Show (Command apiTag 'FireAndForget)
  ) =>
  o (Message apiTag) ->
  Command apiTag 'FireAndForget ->
  m Bool
cast obox !msg =
  MessageBox.deliver obox (NonBlocking msg)

-- | Enqueue a 'Blocking' 'Message' into an 'MessageBox.IsInput' and wait for the
-- response.
--
-- The receiving process must use 'replyTo'  with the 'ReplyBox'
-- received along side the 'Command' in the 'Blocking'.
call ::
  ( HasCallIdCounter env,
    MonadReader env m,
    MonadUnliftIO m,
    MessageBox.IsInput o,
    Show (Command apiTag ( 'Return result))
  ) =>
  o (Message apiTag) ->
  Command apiTag ( 'Return result) ->
  Int ->
  m (Either CommandError result)
call !obox !pdu !timeoutMicroseconds = do
  !callId <- CallId.takeNext
  !resultVar <- newEmptyTMVarIO
  !sendSuccessful <- do
    let !rbox = MkReplyBox resultVar callId
    let !msg = Blocking pdu rbox
    MessageBox.deliver obox msg
  if not sendSuccessful
    then return (Left (CouldNotEnqueueCommand callId))
    else do
      timedOutVar <- registerDelay timeoutMicroseconds
      atomically $
        snd <$> takeTMVar resultVar
          <|> ( do
                  readTVar timedOutVar >>= checkSTM
                  return (Left (BlockingCommandTimedOut callId))
              )

-- | This is called from the callback passed to 'handleMessage'.
-- When handling a 'Blocking' 'Message' the 'ReplyBox' contained
-- in the message contains the 'TMVar' for the result, and this
-- function puts the result into it.
{-# INLINE replyTo #-}
replyTo :: (MonadUnliftIO m) => ReplyBox a -> a -> m ()
replyTo MkReplyBox {_replyBoxCallId = !callId, _replyBox = !replyBox} !message =
  atomically $ putTMVar replyBox (callId, Right message)

-- | Enqueue a 'Blocking' 'Message' into an 'MessageBox.IsInput'.
--
-- The result can be obtained by 'waitForReply'.
--
-- The receiving process must use 'replyTo'  with the 'ReplyBox'
-- received along side the 'Command' in the 'Blocking'.
enqueueCall ::
  ( HasCallIdCounter env,
    MonadReader env m,
    MonadUnliftIO m,
    MessageBox.IsInput o,
    Show (Command apiTag ( 'Return result))
  ) =>
  o (Message apiTag) ->
  Command apiTag ( 'Return result) ->
  m (PendingReply result)
enqueueCall = error "TODO"

-- | Pass on the call to another process.
--
-- Used to implement dispatcher processes.
--
-- Returns 'True' if the 'MessageBox.deliver' operation was
-- successful.
{-# INLINE delegateCall #-}
delegateCall ::
  ( MonadUnliftIO m,
    MessageBox.IsInput o,
    Show (Command apiTag ( 'Return r))
  ) =>
  o (Message apiTag) ->
  Command apiTag ( 'Return r) ->
  ReplyBox r ->
  m Bool
delegateCall !o !c !r =
  MessageBox.deliver o (Blocking c r)

-- | The result of 'enqueueCall'.
-- Use 'waitForReply' or 'tryTakeReply'.
newtype PendingReply r = MkPendingReply ()

-- | Wait for the reply of a 'Blocking' 'Message'
-- sent by 'enqueueCall'.
{-# INLINE waitForReply #-}
waitForReply ::
  MonadUnliftIO m =>
  PendingReply r ->
  -- | The time in micro seconds to wait
  -- before returning 'Left' 'BlockingCommandTimedOut'
  Int ->
  m (Either CommandError result)
waitForReply _ = error "TODO"

-- | If a reply for an 'enqueueCall' operation is available
-- return it, otherwise return 'Nothing'.
{-# INLINE tryTakeReply #-}
tryTakeReply ::
  MonadUnliftIO m =>
  PendingReply r ->
  m (Maybe (Either CommandError result))
tryTakeReply = error "TODO"
