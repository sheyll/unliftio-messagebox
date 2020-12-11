{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import Control.Monad (replicateM)
import Criterion.Main (defaultMain)
import Data.Foldable (foldrM)
import Data.Semigroup (All (All), Semigroup (stimes))
import Protocol.MessageBox
  ( InBox,
    OutBox,
    OutBoxFailure,
    OutBoxSuccess,
    createInBox,
    createOutBoxForInbox,
    receive,
    trySend,
    trySendAndWait,
  )
import UnliftIO (Conc, MonadUnliftIO, conc, runConc)
import Criterion.Types
    ( bench, bgroup, nfAppIO )

main =
  defaultMain
    [ bgroup
        "MessageBox"
        [ bgroup
            "using trySend"
            [ bench
                "1 src/1 msgs/1 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1, 1, 1)
                ),
              bench
                "1 src/1000 msgs/100 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1, 1000, 100)
                ),
              bench
                "1 src/100 msgs/1000 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1, 100, 1000)
                ),
              bench
                "1 src/10 msgs/10000 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1, 10, 10000)
                ),
              bench
                "1 src/1 msgs/100000 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1, 1, 100000)
                ),
              bench
                "1 src/100000 msgs/1 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1, 100000, 1)
                ),
              -- multiple senders
              bench
                "10 src/100 msgs/100 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (10, 100, 100)
                ),
              bench
                "100 src/100 msgs/10 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (100, 100, 100)
                ),
              bench
                "1000 src/100 msgs/1 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (1000, 100, 1)
                ),
              bench
                "100000 src/1 msgs/1 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeath (100000, 1, 1)
                )
            ],
          bgroup
            "using trySendAndWait"
            [ bench
                "1 src/1000 msgs/100 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (1, 1000, 100)
                ),
              bench
                "1 src/100 msgs/1000 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (1, 100, 1000)
                ),
              bench
                "1 src/10 msgs/10000 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (1, 10, 10000)
                ),
              -- multiple senders
              bench
                "10 src/100 msgs/100 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (10, 100, 100)
                ),
              bench
                "100 src/100 msgs/10 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (100, 100, 10)
                ),
              bench
                "10 src/10 msgs/1000 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (10, 10, 1000)
                ),
              bench
                "1000 src/10 msgs/10 dests"
                ( nfAppIO senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting (1000, 10, 10)
                )
            ]
        ]
    ]

senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting ::
  MonadUnliftIO m => (Int, Int, Int) -> m All
senderSendsMessagesToReceiversAndAwaitsDeathWithWaiting =
  senderSendsMessagesToReceivers (trySendAndWait 1000)

senderSendsMessagesToReceiversAndAwaitsDeath ::
  MonadUnliftIO m => (Int, Int, Int) -> m All
senderSendsMessagesToReceiversAndAwaitsDeath =
  senderSendsMessagesToReceivers trySend

senderSendsMessagesToReceivers ::
  MonadUnliftIO m => TrySendFun m -> (Int, Int, Int) -> m All
senderSendsMessagesToReceivers trySendImpl (!nSenders, !nMsgs, !nReceivers) = do
  allThreads <-
    do
      (receiverThreads, receiverOutBoxes) <- startReceivers nMsgs nReceivers
      let !senderThreads = stimes nSenders (conc (senderLoop trySendImpl receiverOutBoxes))
      return (senderThreads <> receiverThreads)
  runConc allThreads

type TrySendFun f = (forall x. OutBox x -> x -> f (Either OutBoxFailure OutBoxSuccess))

senderLoop :: MonadUnliftIO f => TrySendFun f -> [OutBox TestMsg] -> f All
senderLoop _ [] = pure (All True)
senderLoop trySendImpl !rs = foldrM go [] rs >>= senderLoop trySendImpl
  where
    go !outBox !outBoxesAcc = do
      !result <- trySendImpl outBox (MkTestMsg False)
      case result of
        Left _ -> return outBoxesAcc
        Right _ -> return (outBox : outBoxesAcc)

newtype TestMsg = MkTestMsg {_poison :: Bool}

startReceivers ::
  MonadUnliftIO m =>
  Int ->
  Int ->
  m (Conc m All, [OutBox TestMsg])
startReceivers nMsgs nReceivers = do
  inBoxes <- replicateM nReceivers (createInBox 1024)
  let receivers = foldMap (conc . receiverLoop nMsgs) inBoxes
  outBoxes <- traverse createOutBoxForInbox inBoxes
  return (receivers, outBoxes)

receiverLoop :: MonadUnliftIO m => Int -> InBox TestMsg -> m All
receiverLoop workLeft inBox
  | workLeft < 1 = pure (All True)
  | otherwise = do
    !msg <- receive inBox
    case msg of
      _ -> receiverLoop (workLeft - 1) inBox

