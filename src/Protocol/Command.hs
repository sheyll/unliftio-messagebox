-- | This provides the /Command Pattern/.
--
-- This module helps to build corrent RPC-style communication.
--
-- Nothing new is invented here, this just wrapping existing
-- libraries with rememberable, use case specific names.
--
-- It is a thin layer on async, STM and unliftio.
module Protocol.Command
  ( Message (),
    Command,
    ReturnType (..),
    CallId (),
    ReplyBox (),
    CommandError (..),
    cast,
    call,
    handleMessage,
    replyTo,
  )
where

import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import Protocol.Fresh
  ( HasCounterVar,
    fresh,
  )
import qualified Protocol.BoundedMessageBox as MessageBox
import System.Mem.Weak (Weak)
import UnliftIO
  ( MonadUnliftIO,
    TMVar,
    mkWeakTMVar,
    newEmptyTMVarIO,
  )

-- | This family allows declaration of valid
-- command messages of a user defined api.
--
-- The api is discerned based upon a user defined (phantom-)
-- type.
-- paired with a 'ReturnType' that declares if and what
-- response is valid for a message.
--
-- Example:
--
-- >
-- > type LampId = Int
-- >
-- > data instance Command LightControl r where
-- >   GetLamps :: Command LigthControl (Return [LampId])
-- >   SwitchOn :: LampId -> Command LigthControl FireAndForget
-- >
-- > data LightControl -- the phantome type ise bu nnut
-- >
--
-- The first type parameter is a phantom type - probably uninhabited -
-- indicating a set of messages commonly describing an API, or
-- api.
--
-- The second type parameter indicates if a message requires the
-- receiver to send a reply back to the blocked and waiting
-- sender, or if no reply is necessary.
--
-- Since 'Command' instances can be generalised algebraic data types
-- the specific 'ReturnType'
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
    Command api ( 'Return result) ->
    ReplyBox result ->
    Message api
  -- | If the 'Command' has a 'ReturnType' of 'FireAndForget'
  -- it has fire-and-forget semantics.
  --
  -- The smart constructor 'cast' can be used to
  -- this message.
  NonBlocking ::
    Command api 'FireAndForget ->
    Message api

-- | This is like 'OutBox', it can be used
-- by the receiver of a 'Blocking'
-- to either send a reply using 'reply'
-- or to fail/abort the request using 'sendRequestError'
data ReplyBox a = MkReplyBox
  { _replyBox :: Weak (TMVar (InternalReply a)),
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

-- | An identifier value every command send by 'call's.
newtype CallId = MkCallId Int
  deriving newtype (Eq, Ord)

-- | Enqueue a 'NonBlocking' 'Message' into an 'OutBox'.
-- This is just for symetry to 'call', this is
-- equivalent to: @\obox -> MessageBox.tryToDeliver obox . NonBlocking@
--
-- The
cast ::
  MonadUnliftIO m =>
  MessageBox.OutBox (Message api) ->
  Command api 'FireAndForget ->
  m Bool
cast obox !msg =
  MessageBox.tryToDeliver obox (NonBlocking msg)

-- | Enqueue a 'Blocking' 'Message' into an 'OutBox' and wait for the
-- response.
--
-- The receiving process must use 'replyTo'  with the 'ReplyBox'
-- received along side the 'Command' in the 'Blocking'.
call ::
  ( HasCounterVar CallId env,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  MessageBox.OutBox (Message api) ->
  Command api ( 'Return result) ->
  m (Either CommandError result)
call !obox !pdu = do
  !callId <- fresh
  !resultVar <- newEmptyTMVarIO
  !sendSuccessful <- do
    !weakResultVar <- mkWeakTMVar resultVar (pure ())
    let !rbox = MkReplyBox weakResultVar callId
    let !msg = Blocking pdu rbox
    MessageBox.tryToDeliver obox msg
  if not sendSuccessful
    then return (Left (CouldNotEnqueueCommand callId))
    else error "TODO"

-- | Receive a 'NonBlocking' or a 'Blocking'.
--
-- Block until the reBlockingply is received.
-- If for the given time, no answer was
-- received, return 'BlockingCommandTimedOut'.
handleMessage :: ()
handleMessage = error "TODO"

replyTo :: ()
replyTo = error "TODO"