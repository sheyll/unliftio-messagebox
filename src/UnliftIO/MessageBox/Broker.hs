module UnliftIO.MessageBox.Broker
  ( spawnBroker,
    BrokerConfig (..),
    MessageToBrokerAction,
    BrokerAction (..),
    InitWorker,
    TerminateWorker (..),
    WorkerLoop,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor (void, ($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import UnliftIO
  (Async,
    MonadUnliftIO,
    SomeException,
    async,
    asyncWithUnmask,
    atomically,
    cancel,
    checkSTM,
    mapConcurrently_,
    onException,
    readTVar,
    registerDelay,
    tryAny,
    waitCatch,
    waitCatchSTM,
  )
import UnliftIO.Concurrent
  ( yield,
  )
import UnliftIO.MessageBox.Class
  ( IsInput (deliver),
    IsMessageBox (Input, newInput, receive),
    IsMessageBoxArg (MessageBox, newMessageBox),
  )
import Control.Monad (join)

-- | Spawn a process that receives messages dispatches them to /workers/.
--
-- The process runs as long as 'Just' messages come in, and stops
-- when 'Nothing' is received.
--
-- Type parameters:
--  * @m@ is the base monad.
--  * @k@ is the unique _key_ that identifies workers. For example a UDP port, or a database ID.
--  * @brokerBoxArg@ is a type which has an 'IsMessageBoxArg' instance, used to create the
--                   'MessageBox' of the broker process.
--  * @f@ is a type which has an 'IsMessageBoxArg' instance, used to create the
--       'MessageBox' of the worker processes.
--  * @w'@ is the type of the incoming messages, that are passed to 'MessageToBrokerAction'.
--  * @w@ is the type of messages that the broker creates from @w'@ values using
--    the 'MessageToBrokerAction' and then passes to the worker processes.
--  * @a@ is the result that the worker returns when it exits.
spawnBroker ::
  forall brokerBoxArg k w' w f m.
  ( MonadUnliftIO m,
    Ord k,
    IsMessageBoxArg brokerBoxArg,
    IsMessageBoxArg f
  ) =>
  brokerBoxArg ->
  BrokerConfig k w' w f m () ->
  m (Either SomeException (Input (MessageBox brokerBoxArg) (Maybe w'), Async ()))
spawnBroker brokerBoxArg config = do
  brokerA <- async $ do
      mBrokerBox <- tryAny (do 
          b <- newMessageBox brokerBoxArg
          i <- newInput b
          return (b, i))
      case mBrokerBox of
        Left er -> return (Left er)
        Right (brokerBox, brokerInp) -> do          
          aInner <- asyncWithUnmask $ \unmaskInner ->
            brokerLoop unmaskInner brokerBox config (0, Map.empty)
          return (Right (brokerInp, aInner))
  join <$> waitCatch brokerA
  

-- | Dispatch incoming messages to brokerState.
-- The received messages are discriminated by an id.
data BrokerConfig k w' w f m a = MkBrokerConfig
  { messageToBrokerAction :: MessageToBrokerAction w' k w,
    workerMessageBoxArg :: f,
    initWorker :: InitWorker k w f m a,
    terminateWorker :: TerminateWorker k w f m,
    workerLoop :: WorkerLoop k w f m a
  }

-- | A function to extract the key and the 'BrokerAction' from a
-- message.
type MessageToBrokerAction w' k w = w' -> (k, BrokerAction w)

-- | The action that the broker has to take for in incoming message.
data BrokerAction w
  = -- | Start a new worker process and pass the message to the new worker.
    --   If the the key already exists, pass the message to the existing worker.
    StartAndForward (Maybe w)
  | -- | Forward a message to a worker and stop it afterwards.
    -- Silently ignore if no worker for the key exists.
    ForwardAndStop (Maybe w)
  | -- | Pass a message to an existing worker.
    -- Silently ignore if no worker for the key exists.
    Forward w

-- | Create and initialize a 'Worker'. This is required because a
-- worker is impure, and might use mutable references e.g. IORefs,
-- that need to be initialized for every new job.
type InitWorker k w f m a = k -> Input (MessageBox f) w -> m (WorkerLoop k w f m a)

-- | This action should contain a loop that processes the incoming messages
-- of worker until a stop message is received or the 'TerminateWorker' for this
-- worker was called.
type WorkerLoop k w f m a = k -> MessageBox f w -> m a

-- | A configuration that governs how the broker stops a worker.
data TerminateWorker k w f m = MkTerminateWorker
  { -- | An action that is called to cause a worker to stop
    --   and return.
    --   The @key@ and the 'Input' of the workers 'MessageBox' are
    --   passed to the action, so the action may 'deliver' a
    --   shutdown message.
    terminateWorkerAction :: !(k -> Input (MessageBox f) w -> m ()),
    -- | Time in micro seconds that the broker will wait after the
    --   'terminateWorkerAction' action has returned before brutally
    --   calling 'cancel' and the worker.
    terminateWorkerTimeout :: !Int
  }

brokerLoop ::
  ( MonadUnliftIO m,
    Ord k,
    IsMessageBoxArg f,
    IsMessageBox msgBox
  ) =>
  (forall x. m x -> m x) ->
  msgBox (Maybe w') ->
  BrokerConfig k w' w f m a ->
  (Int, BrokerState k w f a) ->
  m ()
brokerLoop unmask brokerBox config (receiveRetries, brokerState)
  | receiveRetries >= 3 =
    void (shutdownAllWorkers config brokerState)
  | otherwise =
    ( unmask
        ( receive brokerBox
            >>= traverse
              ( maybe
                  (return (Left brokerState))
                  (fmap Right . dispatchMessage config brokerState)
              )
        )
        `onException` shutdownAllWorkers config brokerState
    )
      >>= maybe
        ( yield >> brokerLoop unmask brokerBox config (receiveRetries + 1, brokerState)
        )
        ( either
            (shutdownAllWorkers config)
            (brokerLoop unmask brokerBox config . (0,))
        )

dispatchMessage ::
  (Ord k, IsMessageBoxArg f, MonadUnliftIO m) =>
  BrokerConfig k w' w f m a ->
  BrokerState k w f a ->
  w' ->
  m (BrokerState k w f a)
dispatchMessage config brokerState w' =
  case messageToBrokerAction config w' of
    (k, StartAndForward mw) -> do
      mBrokerState1 <- spawnWorker k config brokerState
      case mBrokerState1 of
        Nothing ->
          return brokerState
        Just brokerState1 -> do
          case mw of
            Nothing ->
              return brokerState1
            Just w -> do
              (msgSent, brokerState2) <- deliverToWorker k w config brokerState1
              if msgSent
                then return brokerState2
                else do
                  _brokerState3 <- shutdownWorker k config brokerState
                  return brokerState
    (k, ForwardAndStop mw) -> do
      brokerState1 <-
        case mw of
          Nothing ->
            return brokerState
          Just w ->
            snd <$> deliverToWorker k w config brokerState
      mBrokerState2 <- shutdownWorker k config brokerState1
      return (maybe brokerState1 snd mBrokerState2)
    (k, Forward w) -> do
      (_, brokerState2) <- deliverToWorker k w config brokerState
      return brokerState2

spawnWorker ::
  (Ord k, IsMessageBoxArg f, MonadUnliftIO m) =>
  k ->
  BrokerConfig k w' w f m a ->
  BrokerState k w f a ->
  m (Maybe (BrokerState k w f a))
spawnWorker k config brokerState =
  case Map.lookup k brokerState of
    Just _ ->
      return (Just brokerState)
    Nothing -> do
      workerBox <- newMessageBox (workerMessageBoxArg config)
      workerInput <- newInput workerBox
      tryAny (initWorker config k workerInput)
        >>= either
          (const (tryAny (terminateWorkerAction (terminateWorker config) k workerInput) $> Nothing))
          ( \workerLoop -> do
              workerAsync <- async (workerLoop k workerBox)
              return
                ( Just
                    ( Map.insert
                        k
                        (workerInput, workerAsync)
                        brokerState
                    )
                )
          )

deliverToWorker ::
  (Ord k, IsMessageBoxArg f, MonadUnliftIO m) =>
  k ->
  w ->
  BrokerConfig k w' w f m a ->
  BrokerState k w f a ->
  m (Bool, BrokerState k w f a)
deliverToWorker k w config brokerState =
  maybe
    (return (False, brokerState))
    deliverHelper
    (Map.lookup k brokerState)
  where
    deliverHelper (wIn, _) = do
      deliverRes <- tryAny (deliver wIn w)
      case deliverRes of
        Right x ->
          return (x, brokerState)
        Left _err ->
          (False,) . maybe brokerState snd
            <$> shutdownWorker k config brokerState

shutdownWorker ::
  (Ord k, MonadUnliftIO m) =>
  k ->
  BrokerConfig k w' w f m a ->
  BrokerState k w f a ->
  m (Maybe (Result a, BrokerState k w f a))
shutdownWorker k config brokerState =
  case Map.lookup k brokerState of
    Nothing ->
      return Nothing
    Just w -> do
      res <- shutdownWorkerHelper config k w
      return (Just (res, Map.delete k brokerState))

shutdownAllWorkers ::
  (MonadUnliftIO m) =>
  BrokerConfig k w' w f m a ->
  BrokerState k w f a ->
  m ()
shutdownAllWorkers config brokerState =
  mapConcurrently_
    (uncurry (shutdownWorkerHelper config))
    (Map.assocs brokerState)

shutdownWorkerHelper ::
  (MonadUnliftIO m) =>
  BrokerConfig k w' w f m a ->
  k ->
  Worker f w a ->
  m (Result a)
shutdownWorkerHelper config k (workerInput, workerAsync) =
  tryAny (terminateWorkerAction (terminateWorker config) k workerInput)
    >>= either
      (const (pure Nothing))
      ( const $ do
          timeoutVar <- registerDelay (terminateWorkerTimeout (terminateWorker config))
          atomically
            ( (Just <$> waitCatchSTM workerAsync)
                <|> do
                  isTimeUp <- readTVar timeoutVar
                  checkSTM isTimeUp
                  return Nothing
            )
      )
    >>= maybe
      ( do
          cancel workerAsync
          waitCatch workerAsync
      )
      return

type Worker f w a = (Input (MessageBox f) w, Async a)

type Result a = Either SomeException a

type BrokerState k w f a = Map k (Worker f w a)
