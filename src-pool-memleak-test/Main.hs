module Main (main) where

import Control.Monad (forM_, replicateM_, void, when)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    cancel,
    mapConcurrently_,
    tryAny,
  )
import UnliftIO.Concurrent
  ( MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import UnliftIO.MessageBox
  ( BlockingUnlimited (BlockingUnlimited),
    IsInput (deliver, deliver_),
    IsMessageBox (newInput, receive),
    IsMessageBoxArg (newMessageBox),
    Multiplexed (..),
    Pool (..),
    PoolWorkerCallback (..),
    spawnPool,
  )

main :: IO ()
main = do
  args <- getArgs
  let (rounds, repetitions, nWorkMessages) =
        case args of
          [n', r', w'] -> (read n', read r', read w')
          [n', r'] -> (read n', read r', 10)
          [n'] -> (read n', 1, 10)
          _ -> (1, 1, 10)
  putStrLn ""
  putStrLn ""
  putStrLn
    ( "Benchmark: Running "
        ++ show repetitions
        ++ " times a benchmark with "
        ++ show rounds
        ++ " active processes, each processing "
        ++ show nWorkMessages
        ++ " work messages."
    )
  putStrLn ""
  putStrLn ""
  bench repetitions rounds nWorkMessages

newtype Key = Key Int deriving newtype (Eq, Ord, Show)

data Msg = Start | Work | SyncWork (MVar Int) | Stop

{-# NOINLINE bench #-}

-- | Start @bSize@ clients that work 10000 work units.
bench :: MonadUnliftIO m => Int -> Int -> Int -> m ()
bench repetitions bSize nWorkMessages = do
  resultBox <- newMessageBox BlockingUnlimited
  resultBoxIn <- newInput resultBox
  -- start a pool
  x <-
    spawnPool
      BlockingUnlimited
      BlockingUnlimited
      (MkPoolWorkerCallback (enterWorkLoop resultBoxIn))
  case x of
    Left err ->
      liftIO (putStrLn ("Error: " ++ show err))
    Right pool -> do
      forM_ [1 .. repetitions] $ \ !rep -> do
        liftIO $ do
          putStrLn ""
          putStrLn ("================= BEGIN (rep: " ++ show rep ++ ") ================")
          putStrLn ""
        do
          let groupSize = 1000
          mapConcurrently_
            (traverse_ (sendWork nWorkMessages (poolInput pool) . Key))
            ( [groupSize * ((bSize - 1) `div` groupSize) .. bSize - 1] :
                [ [groupSize * x0 .. groupSize * x0 + (groupSize - 1)]
                  | x0 <- [0 .. ((bSize - 1) `div` groupSize) - 1]
                ]
            )

          liftIO (putStrLn "Waiting for results: ")
          printResults bSize resultBox
        liftIO $ do
          putStrLn ""
          putStrLn ("================= DONE (rep: " ++ show rep ++ ") ================")
      cancel (poolAsync pool)

{-# NOINLINE printResults #-}
printResults ::
  (IsMessageBox box, MonadUnliftIO m) =>
  Int ->
  box (Key, Int) ->
  m ()
printResults bSize box
  | bSize <= 0 = return ()
  | otherwise =
    receive box
      >>= maybe
        (liftIO (putStrLn "done!"))
        ( \res@(Key k, _) -> do
            when
              (k `mod` 1000 == 0)
              (liftIO (putStrLn ("Result: " ++ show res)))
            printResults (bSize - 1) box
        )

{-# NOINLINE sendWork #-}
sendWork ::
  (IsInput input, MonadUnliftIO m) =>
  Int ->
  input (Multiplexed Key (Maybe Msg)) ->
  Key ->
  m ()
sendWork nWorkMessages poolBoxIn k@(Key x) = do
  when
    (x `mod` 1000 == 0)
    (liftIO (putStrLn ("Delivering Messages for: " ++ show k)))
  void $ deliver poolBoxIn (Initialize k (Just (Just Start)))
  when
    (x `mod` 1000 == 0)
    (liftIO (putStrLn ("Delivering Sync Work Messages for: " ++ show k)))
  do
    resRef <- newEmptyMVar
    replicateM_
      nWorkMessages
      ( do
          deliver_ poolBoxIn (Dispatch k (Just (SyncWork resRef)))
          res <- takeMVar resRef
          when
            (x `mod` 1000 == 0 && res `mod` 1000 == 0)
            ( liftIO
                ( putStrLn
                    ( "Current counter of: "
                        ++ show k
                        ++ " is: "
                        ++ show res
                    )
                )
            )
      )
  when
    (x `mod` 1000 == 0)
    (liftIO (putStrLn ("Delivering Async Work Messages for: " ++ show k)))
  replicateM_
    nWorkMessages
    ( deliver_ poolBoxIn (Dispatch k (Just Work))
    )

  --  replicateM_ nWorkMessages (deliver poolBoxIn (Dispatch k (Just Work)))
  void $ deliver poolBoxIn (Dispatch k (Just Stop))
  when
    (x `mod` 1000 == 0)
    (liftIO (putStrLn ("Delivered all Messages for: " ++ show k)))

{-# NOINLINE enterWorkLoop #-}
enterWorkLoop ::
  (MonadUnliftIO m, IsMessageBox box, IsInput input) =>
  input (Key, Int) ->
  Key ->
  box Msg ->
  m ()
enterWorkLoop resultBoxIn (Key k) box =
  tryAny (receive box)
    >>= ( \case
            Left ex ->
              liftIO
                ( putStrLn
                    ( "Receive threw exception for worker: "
                        ++ show k
                        ++ " "
                        ++ show ex
                    )
                )
            Right Nothing ->
              liftIO (putStrLn ("Receive failed for worker: " ++ show k))
            Right (Just Start) -> do
              when
                (k `mod` 1000 == 0)
                (liftIO (putStrLn ("Started: " ++ show k)))
              workLoop 0
            Right (Just Work) ->
              liftIO (putStrLn ("Got unexpected Work: " ++ show k))
            Right (Just (SyncWork ref)) -> do
              liftIO (putStrLn ("Got unexpected SyncWork: " ++ show k))
              putMVar ref 0
            Right (Just Stop) -> do
              liftIO (putStrLn ("Got unexpected Stop: " ++ show k))
        )
  where
    workLoop !counter =
      tryAny (receive box)
        >>= \case
          Left ex ->
            liftIO
              ( putStrLn
                  ( "Receive threw exception for worker: "
                      ++ show k
                      ++ " "
                      ++ show ex
                  )
              )
          Right Nothing ->
            liftIO (putStrLn ("Receive failed for worker: " ++ show k))
          Right (Just Start) -> do
            liftIO (putStrLn ("Re-start: " ++ show k))
            workLoop 0
          Right (Just Work) ->
            workLoop (counter + 1)
          Right (Just (SyncWork ref)) -> do
            putMVar ref counter
            -- timeout 5000000 (putMVar ref counter)
            --   >>= maybe
            --     (liftIO (putStrLn "SyncWork timeout putting the result"))
            --     (const (return ()))
            workLoop (counter + 1)
          Right (Just Stop) -> do
            void (deliver resultBoxIn (Key k, counter))
            when
              (k `mod` 1000 == 0)
              (liftIO (putStrLn ("Stopped: " ++ show k)))
