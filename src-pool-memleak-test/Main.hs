module Main (main) where

import Control.Monad (forM_, replicateM_, void, when)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    cancel,
  )
import UnliftIO.MessageBox
  ( BlockingUnlimited (BlockingUnlimited),
    IsInput (deliver),
    IsMessageBox (newInput, receive, receiveAfter),
    IsMessageBoxArg (newMessageBox),
    Multiplexed (..),
    Pool (..),
    PoolWorkerCallback (..),
    spawnPool,
  )

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
          traverse_ (sendWork (poolInput pool) . Key) [0 .. bSize - 1]
          liftIO (putStrLn "Waiting for results: ")
          printResults resultBox
        liftIO $ do
          putStrLn ""
          putStrLn ("================= DONE (rep: " ++ show rep ++ ") ================")
      cancel (poolAsync pool)

{-# NOINLINE printResults #-}
printResults :: (IsMessageBox box, MonadUnliftIO m) => box (Key, Int) -> m ()
printResults box =
  receiveAfter box 500000
    >>= maybe
      (liftIO (putStrLn "done!"))
      ( \res@(Key k, _) -> do
          when
            (k `mod` 1000 == 0)
            (liftIO (putStrLn ("Result: " ++ show res)))
          printResults box
      )

{-# NOINLINE sendWork #-}
sendWork ::
  (IsInput input, MonadUnliftIO m) =>
  input (Multiplexed Key (Maybe Msg)) ->
  Key ->
  m ()
sendWork poolBoxIn k@(Key x) = do
  when
    (x `mod` 1000 == 0)
    (liftIO (putStrLn ("Delivering Messages for: " ++ show k)))
  void $ deliver poolBoxIn (Initialize k (Just (Just Start)))
  replicateM_ 10 (deliver poolBoxIn (Dispatch k (Just Work)))
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
  receive box
    >>= traverse_
      ( \case
          Start -> do
            when
              (k `mod` 1000 == 0)
              (liftIO (putStrLn ("Started: " ++ show k)))
            workLoop 0
          Work ->
            liftIO (putStrLn ("Got unexpected Work: " ++ show k))
          Stop -> do
            when
              (k `mod` 1000 == 0)
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
