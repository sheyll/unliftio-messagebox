{-# LANGUAGE StrictData #-}

-- | Abstractions for the definition of
-- 'Command' 'Messages', that flow between
module Protocol.Command
  ( Message (..),
    Command,
    ReturnType (..),
    CallId (MkCallId),
    ReplyBox (),
    CommandError (..),
    cast,
    call,
    handleMessage,
    replyTo,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import Protocol.Fresh
  ( HasCounterVar,
    fresh,
  )
import qualified Protocol.MessageBoxClass as MessageBox
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
data family Command protocolTag :: ReturnType -> Type

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

-- | A message valid for some user defined @api@.
--
-- The @api@ tag (phantom-) type defines the
-- messages allowed here, declared by the instance of
-- 'Command' for 'api'.
data Message api where
  -- | Wraps a 'Command' with a 'ReturnType' of 'Return' @result@.
  --
  -- Such a message can formed by using 'call'.
  --
  -- A 'Blocking' contains a 'ReplyBox' that can be
  -- used to send the reply to the other process
  -- blocking on 'call'
  Blocking ::
    Show (Command api ( 'Return result)) =>
    Command api ( 'Return result) ->
    ReplyBox result ->
    Message api
  -- | If the 'Command' has a 'ReturnType' of 'FireAndForget'
  -- it has fire-and-forget semantics.
  --
  -- The smart constructor 'cast' can be used to
  -- this message.
  NonBlocking ::
    (Show (Command api 'FireAndForget)) =>
    Command api 'FireAndForget ->
    Message api

instance Show (Message api) where
  showsPrec d (NonBlocking !m) =
    showParen (d >= 9) (showString "NonBlocking " . showsPrec 9 m)
  showsPrec d (Blocking !m (MkReplyBox _ !callId)) =
    showParen (d >= 9) (showString "Blocking " . showsPrec 9 m . showChar ' ' . showsPrec 9 callId)

-- | This is like 'OutBox', it can be used
-- by the receiver of a 'Blocking'
-- to either send a reply using 'reply'
-- or to fail/abort the request using 'sendRequestError'
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
  -- 'MessageBox.OutBox'
  CouldNotEnqueueCommand :: CallId -> CommandError
  -- | The request has failed /for reasons/.
  BlockingCommandFailure :: CallId -> CommandError
  -- | Timeout waiting for the result.
  BlockingCommandTimedOut :: CallId -> CommandError
  deriving stock (Show, Eq)

-- | An identifier value every command send by 'call's.
newtype CallId = MkCallId Int
  deriving newtype (Eq, Ord, Show)

-- | Enqueue a 'NonBlocking' 'Message' into an 'OutBox'.
-- This is just for symetry to 'call', this is
-- equivalent to: @\obox -> MessageBox.tryToDeliver obox . NonBlocking@
--
-- The
cast ::
  ( MonadUnliftIO m,
    MessageBox.IsOutBox o,
    Show (Command api 'FireAndForget)
  ) =>
  o (Message api) ->
  Command api 'FireAndForget ->
  m Bool
cast obox !msg =
  MessageBox.deliver obox (NonBlocking msg)

-- | Enqueue a 'Blocking' 'Message' into an 'MessageBox.IsOutBox' and wait for the
-- response.
--
-- The receiving process must use 'replyTo'  with the 'ReplyBox'
-- received along side the 'Command' in the 'Blocking'.
call ::
  ( HasCounterVar CallId env,
    MonadReader env m,
    MonadUnliftIO m,
    MessageBox.IsOutBox o,
    Show (Command api ( 'Return result))
  ) =>
  o (Message api) ->
  Command api ( 'Return result) ->
  Int ->
  m (Either CommandError result)
call !obox !pdu !timeoutMicroseconds = do
  !callId <- fresh
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

-- | Receive a 'NonBlocking' or a 'Blocking'.
--
-- Block until the reply is received.
-- If for the given time, no answer was
-- received, return 'BlockingCommandTimedOut'.
handleMessage ::
  (MonadUnliftIO m, MessageBox.IsInBox inbox) =>
  inbox (Message api) ->
  (Message api -> m b) ->
  m (Maybe b)
handleMessage !inbox !onMessage = do
  !maybeMessage <- MessageBox.receive inbox
  case maybeMessage of
    Nothing -> pure Nothing
    Just !message -> do
      Just <$> onMessage message

replyTo :: (MonadUnliftIO m) => ReplyBox a -> a -> m ()
replyTo MkReplyBox {_replyBoxCallId = !callId, _replyBox = !replyBox} !message =
  atomically $ putTMVar replyBox (callId, Right message)