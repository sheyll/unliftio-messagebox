module Main (main) where

import Control.Monad (forM_, replicateM_, void, when)
import Data.Foldable (traverse_)
import Data.Function (fix)
import System.Environment (getArgs)
import UnliftIO
  ( Async,
    MVar,
    MonadIO (liftIO),
    MonadUnliftIO,
    SomeException,
    async,
    cancel,
    finally,
    mapConcurrently_,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryReadMVar,
  )
import UnliftIO.MessageBox
  ( BlockingUnlimited (BlockingUnlimited),
    IsInput (deliver),
    IsMessageBox (Input, newInput, receive, receiveAfter),
    IsMessageBoxArg (MessageBox, newMessageBox),
  )
import UnliftIO.MessageBox.Broker

main :: IO ()
main = do
  args <- getArgs
  let (rounds, repetitions) =
        case args of
          [n', r'] -> (read n', read r')
          [n'] -> (read n', 1)
          _ -> (1, 1)
  putStrLn ""
  putStrLn ""
  putStrLn
    ( "Benchmark: Running "
        ++ show repetitions
        ++ " times a benchmark with "
        ++ show rounds
        ++ " active processes."
    )
  putStrLn ""
  putStrLn ""
  bench repetitions rounds

newtype Key = Key Int deriving newtype (Eq, Ord, Show)

data Msg = Start | Work | Stop

{-# NOINLINE bench #-}

-- | Start @bSize@ clients that work 10000 work units.
bench :: MonadUnliftIO m => Int -> Int -> m ()
bench repetitions bSize = do
  resultBox <- newMessageBox BlockingUnlimited
  resultBoxIn <- newInput resultBox
  -- start a pool
  x <-
    startPool
      BlockingUnlimited
      BlockingUnlimited
      (MkPoolMemberCallback (enterWorkLoop resultBoxIn))
  case x of
    Left err ->
      liftIO (putStrLn ("Error: " ++ show err))
    Right (poolBoxIn, poolAsync) -> do
      forM_ [1 .. repetitions] $ \ !rep -> do
        liftIO $ do 
          putStrLn ""
          putStrLn ("================= BEGIN (rep: " ++ show rep ++ ") ================")
          putStrLn ""
        do 
          traverse_ (sendWork poolBoxIn . Key) [0 .. bSize - 1]
          liftIO (putStrLn "Waiting for results: ")
          printResults resultBox
        liftIO $ do 
          putStrLn ""
          putStrLn ("================= DONE (rep: " ++ show rep ++ ") ================")
      cancel poolAsync


{-# NOINLINE printResults #-}
printResults :: (IsMessageBox box, MonadUnliftIO m) => box (Key, Int) -> m ()
printResults box =   
    receiveAfter box 500000
      >>= maybe
        (liftIO (putStrLn "done!"))
        ( \res@(Key k, _) -> do
            when (k `mod` 10000 == 0) 
              (liftIO (putStrLn ("Result: " ++ show res)))
            printResults box
        )

{-# NOINLINE sendWork #-}
sendWork ::
  (IsInput input, MonadUnliftIO m) =>
  input (Multiplexed Key (PoolMessage Msg)) ->
  Key ->
  m ()
sendWork poolBoxIn k@(Key x) = do
  when (x `mod` 10000 == 0) 
      (liftIO (putStrLn ("Delivering Messages for: " ++ show k)))
  void $ deliver poolBoxIn (Initialize k (Just (PoolMemberWork Start)))
  replicateM_ 10 (deliver poolBoxIn (Dispatch k (PoolMemberWork Work)))
  void $ deliver poolBoxIn (Dispatch k (PoolMemberWork Stop))
  when (x `mod` 10000 == 0) 
      (liftIO (putStrLn ("Delivered all Messages for: " ++ show k)))

{-# NOINLINE enterWorkLoop #-}
enterWorkLoop ::
  (MonadUnliftIO m, IsMessageBox box, IsInput input) =>
  input (Key, Int) ->
  Key ->
  box Msg ->
  m ()
enterWorkLoop resultBoxIn (Key k) box =
  receive box
    >>= traverse_
      ( \case
          Start -> do
            when (k `mod` 10000 == 0) 
              (liftIO (putStrLn ("Started: " ++ show k)))
            workLoop 0
          Work ->
            liftIO (putStrLn ("Got unexpected Work: " ++ show k))
          Stop -> do
            when (k `mod` 10000 == 0) 
              (liftIO (putStrLn ("Stopped: " ++ show k)))
            liftIO (putStrLn ("Got unexpected Stop: " ++ show k))
      )
  where
    workLoop !counter =
      receive box
        >>= traverse_
          ( \case
              Start -> do
                liftIO (putStrLn ("Re-start: " ++ show k))
                workLoop 0
              Work ->
                workLoop (counter + 1)
              Stop ->
                void (deliver resultBoxIn (Key k, counter))
          )

data PoolConfig init w m = MkPoolConfig
  {
  }

newtype PoolMemberCallback b w k m = MkPoolMemberCallback
  { runMkPoolMemberCallback :: k -> b w -> m ()
  }

data PoolMember box w m = MkPoolMember
  { poolMemberInput :: Input box w,
    poolMemberAsync :: !(Async ())
  }

data PoolMessage w
  = PoolMemberWork w
  | PoolMemberCleanup

startPool ::
  ( MonadUnliftIO m,
    IsMessageBoxArg brokerBoxArg,
    Ord k,
    IsMessageBoxArg boxArg
  ) =>
  brokerBoxArg ->
  boxArg ->
  PoolMemberCallback (MessageBox boxArg) w k m ->
  m
    ( Either
        SomeException
        ( Input (MessageBox brokerBoxArg) (Multiplexed k (PoolMessage w)),
          Async BrokerResult
        )
    )
startPool brokerBoxArg workerBoxArg poolMemberImpl = do
  brInRef <- newEmptyMVar
  let brCfg =
        MkBrokerConfig
          demuxPoolMsg
          dispatchToPoolMember
          (spawnPoolMember workerBoxArg brInRef poolMemberImpl)
          removePoolMember
  spawnBroker brokerBoxArg brCfg
    >>= traverse
      ( \(brIn, brA) -> do
          putMVar brInRef brIn
          return (brIn, brA)
      )

demuxPoolMsg :: Demultiplexer (Multiplexed k (PoolMessage w)) k (PoolMessage w)
demuxPoolMsg = id

dispatchToPoolMember ::
  (MonadUnliftIO m, IsInput (Input b)) =>
  k ->
  PoolMessage w ->
  PoolMember b w m ->
  m (ResourceUpdate (PoolMember b w m))
dispatchToPoolMember _k pMsg pm =
  case pMsg of
    PoolMemberWork w -> do
      helper w
    PoolMemberCleanup -> do
      return (RemoveResource Nothing)
  where
    helper msg = do
      ok <- deliver (poolMemberInput pm) msg
      if not ok
        then do
          return (RemoveResource Nothing)
        else return KeepResource

spawnPoolMember ::
  ( IsMessageBoxArg boxArg,
    MonadUnliftIO m,
    IsInput brokerBoxIn
  ) =>
  boxArg ->
  MVar (brokerBoxIn (Multiplexed k (PoolMessage w))) ->
  PoolMemberCallback (MessageBox boxArg) w k m ->
  k ->
  Maybe (PoolMessage w) ->
  m (PoolMember (MessageBox boxArg) w m)
spawnPoolMember boxArg brInRef pmCb this _mw = do
  inputRef <- newEmptyMVar
  a <- async (go inputRef)
  boxIn <- takeMVar inputRef
  return MkPoolMember {poolMemberInput = boxIn, poolMemberAsync = a}
  where
    go inputRef = do
      b <- newMessageBox boxArg
      boxIn <- newInput b
      putMVar inputRef boxIn
      runMkPoolMemberCallback pmCb this b
        `finally` enqueueCleanup
    enqueueCleanup =
      tryReadMVar brInRef
        >>= traverse_
          ( \brIn ->
              void (deliver brIn (Dispatch this PoolMemberCleanup))
          )

removePoolMember ::
  (MonadUnliftIO m) =>
  k ->
  PoolMember boxArg w m ->
  m ()
removePoolMember _k pm =
  void (cancel (poolMemberAsync pm))
