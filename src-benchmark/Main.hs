{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import Control.Monad ( replicateM )
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import qualified Protocol.BoundedMessageBox as M1
import qualified Protocol.MessageBoxSimpler as M2
import UnliftIO ( conc, runConc, MonadUnliftIO )

main =
  defaultMain
    [ bgroup
        title
        [let 
            noMessages = 10_000
          in
          bgroup
            ("sending " ++ show noMessages ++ " messages")
            [ bench
                "1 sender -> 1 receiver"
                ( nfAppIO impl (1, noMessages, 1)
                ),
              bench
                "1 sender -> 100 receivers"
                ( nfAppIO impl (1, noMessages, 100)
                ),
              bench
                "1 sender -> 1000 receivers"
                ( nfAppIO impl (1, noMessages, 1000)
                ),
              bench
                "1 sender -> 10000 receivers"
                ( nfAppIO impl (1, noMessages, 10000)
                ),
              bench
                "1 sender -> 1 receiver"
                ( nfAppIO impl (1, noMessages, 1)
                ),
              -- multiple senders
              bench
                "10 senders -> 100 receivers"
                ( nfAppIO impl (10, noMessages, 100)
                ),
              bench
                "100 senders -> 10 receivers"
                ( nfAppIO impl (100, noMessages, 100)
                ),
              bench
                "1000 senders -> 1 receiver"
                ( nfAppIO impl (1000, noMessages, 1)
                ),
              bench
                "10000 senders -> 1 receiver"
                ( nfAppIO impl (10000, noMessages, 1)
                )
            ]
        ]
    | (title,impl) <- [("MessageBox Full Featured", nToM_M1), ("Simple Message Box", nToM_M2)]]



newtype TestMsg = MkTestMsg {_someFlag :: Bool}

    
nToM_M1 ::
  MonadUnliftIO m => (Int, Int, Int) -> m ()
nToM_M1 (!nSenders, !nMsgsTotal, !nReceivers) = do
  allThreads <-
    do
      let nMsgsPerReceiver = nMsgsTotal `div` nReceivers
      (receiverThreads, receiverOutBoxes) <- startReceivers nMsgsPerReceiver nReceivers
      let nMsgsPerSender = nMsgsPerReceiver `div` nSenders
      let !senderThreads = stimes nSenders (conc (senderLoop receiverOutBoxes nMsgsPerSender))
      return (senderThreads <> receiverThreads)
  runConc allThreads
  where
    {-# INLINE sendM1 #-}
    sendM1 (!o, !a) = 
      M1.trySendAndWait 5_000_000 o a >>= 
        \case
          Left e -> error ("M1.trySendAndWait failed: " ++ show e)
          Right _ -> return ()

    senderLoop !rs !noMsgs =
      mapM_
        sendM1
        ((,) <$> rs <*> replicate noMsgs (MkTestMsg False))

    startReceivers nMsgs' nReceivers' = do
      inBoxes <- replicateM nReceivers' (M1.createInBox 128)
      let receivers = foldMap (conc . receiverLoop nMsgs') inBoxes
      outBoxes <- traverse M1.createOutBoxForInbox inBoxes
      return (receivers, outBoxes)

    receiverLoop workLeft inBox
      | workLeft < 1 = pure ()
      | otherwise = do 
          _ <- M1.receive inBox 
          receiverLoop (workLeft - 1) inBox

    
nToM_M2 ::
  MonadUnliftIO m => (Int, Int, Int) -> m ()
nToM_M2 (!nSenders, !nMsgsTotal, !nReceivers) = do
  allThreads <-
    do
      let nMsgsPerReceiver = nMsgsTotal `div` nReceivers
      (receiverThreads, receiverOutBoxes) <- startReceivers nMsgsPerReceiver nReceivers
      let nMsgsPerSender = nMsgsPerReceiver `div` nSenders
      let !senderThreads = stimes nSenders (conc (senderLoop receiverOutBoxes nMsgsPerSender))
      return (senderThreads <> receiverThreads)
  runConc allThreads
  where
    {-# INLINE sendM2 #-}
    sendM2 (!o, !a) = 
      M2.trySendAndWait 5_000_000 o a >>= 
        \case
          False -> error (show "M2.trySendAndWait timed out")
          True -> return ()

    senderLoop !rs !noMsgs =
      mapM_
        sendM2
        ((,) <$> rs <*> replicate noMsgs (MkTestMsg False))

    startReceivers nMsgs' nReceivers' = do
      inBoxes <- replicateM nReceivers' (M2.createInBox 128)
      let receivers = foldMap (conc . receiverLoop nMsgs') inBoxes
      outBoxes <- traverse M2.createOutBoxForInbox inBoxes
      return (receivers, outBoxes)

    receiverLoop workLeft inBox
      | workLeft < 1 = pure ()
      | otherwise = do 
          _ <- M2.receive inBox 
          receiverLoop (workLeft - 1) inBox
