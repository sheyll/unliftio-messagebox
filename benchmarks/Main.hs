{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import Control.Monad
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import Protocol.MessageBox
  (trySendAndWaitForever,  InBox,
    OutBox,
    createInBox,
    createOutBoxForInbox,
    receive,
    trySendAndWait,
  )
import UnliftIO

main =
  defaultMain
    [ bgroup
        "MessageBox: n -> m"
        [ bgroup
            "using trySend"
            [ bench
                "1 src/1 msgs/1 dests"
                ( nfAppIO nToM (1, 1, 1)
                ),
              bench
                "1 src/1000 msgs/100 dests"
                ( nfAppIO nToM (1, 1000, 100)
                ),
              bench
                "1 src/100 msgs/1000 dests"
                ( nfAppIO nToM (1, 100, 1000)
                ),
              bench
                "1 src/10 msgs/10000 dests"
                ( nfAppIO nToM (1, 10, 10000)
                ),
              bench
                "1 src/1 msgs/100000 dests"
                ( nfAppIO nToM (1, 1, 100000)
                ),
              bench
                "1 src/100000 msgs/1 dests"
                ( nfAppIO nToM (1, 100000, 1)
                ),
              -- multiple senders
              bench
                "10 src/100 msgs/100 dests"
                ( nfAppIO nToM (10, 100, 100)
                ),
              bench
                "100 src/100 msgs/10 dests"
                ( nfAppIO nToM (100, 100, 100)
                ),
              bench
                "1000 src/100 msgs/1 dests"
                ( nfAppIO nToM (1000, 100, 1)
                ),
              bench
                "100000 src/1 msgs/1 dests"
                ( nfAppIO nToM (100000, 1, 1)
                )
            ]
        ]
    ]

send :: MonadUnliftIO m => OutBox a -> a -> m ()
send !o =
  trySendAndWaitForever o
    >=> ( \case
            Nothing -> void (error "OutBoxClosed")
            _ -> return ()
        )

sendWithTimeout :: MonadUnliftIO m => Int -> OutBox a -> a -> m ()
sendWithTimeout !to !o =
  trySendAndWait to o
    >=> ( \case
            Left e -> void (error (show e))
            _ -> return ()
        )

nToM ::
  MonadUnliftIO m => (Int, Int, Int) -> m ()
nToM =
  senderSendsMessagesToReceivers send --(sendWithTimeout 1000000000)

senderSendsMessagesToReceivers ::
  MonadUnliftIO m => TrySendFun m -> (Int, Int, Int) -> m ()
senderSendsMessagesToReceivers trySendImpl (!nSenders, !nMsgs, !nReceivers) = do
  allThreads <-
    do
      (receiverThreads, receiverOutBoxes) <- startReceivers nMsgs nReceivers
      let !senderThreads = stimes nSenders (conc (senderLoop trySendImpl receiverOutBoxes nMsgs))
      return (senderThreads <> receiverThreads)
  runConc allThreads

type TrySendFun f = (forall x. OutBox x -> x -> f ())

senderLoop :: MonadUnliftIO f => TrySendFun f -> [OutBox TestMsg] -> Int -> f ()
senderLoop trySendImpl !rs !noMsgs =
  mapM_
    (uncurry trySendImpl)
    ((,) <$> rs <*> replicate noMsgs (MkTestMsg False))

newtype TestMsg = MkTestMsg {_poison :: Bool}

startReceivers ::
  MonadUnliftIO m =>
  Int ->
  Int ->
  m (Conc m (), [OutBox TestMsg])
startReceivers nMsgs nReceivers = do
  inBoxes <- replicateM nReceivers (createInBox 1024)
  let receivers = foldMap (conc . receiverLoop nMsgs) inBoxes
  outBoxes <- traverse createOutBoxForInbox inBoxes
  return (receivers, outBoxes)

receiverLoop :: MonadUnliftIO m => Int -> InBox TestMsg -> m ()
receiverLoop workLeft inBox
  | workLeft < 1 = pure ()
  | otherwise = do 
      _ <- receive inBox 
      receiverLoop (workLeft - 1) inBox
