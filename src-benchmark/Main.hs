{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import Control.Monad (replicateM)
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import qualified Protocol.MessageBox as M2
import qualified Protocol.UnboundedMessageBox as Unbounded
import UnliftIO (MonadUnliftIO, conc, runConc)

main =
  defaultMain
    [ bgroup
        title
        [ let noMessages = 10_000
           in bgroup
                ("n=" <> show noMessages)
                [ bench
                    (show senderNo <> " -> " <> show receiverNo)
                    (nfAppIO impl (senderNo, noMessages, receiverNo))
                ]
        ]
      | (senderNo, receiverNo) <-
          [ (1, 1000),
            (10, 100),
            --(1, 2),
            (1, 1),
            --(2, 1),
            (100, 10),
            (1000, 1)
          ],
        (title, impl) <-
          [ ("XL qs=4096 ",   nToM_M2 4096 mkBigMessage),
          ("XL qs=4096 U",   nToM_Unbounded 4096 mkBigMessage),
            ("XL qs=16 ",   nToM_M2 16 mkBigMessage),
            ("XL qs=16 U",   nToM_Unbounded 16 mkBigMessage),
            ("S qs=4096 ",   nToM_M2 4096 mkSmallMessage),
            ("S qs=4096 U",   nToM_Unbounded 4096 mkSmallMessage),
            ("S qs=16 ",   nToM_M2 16 mkSmallMessage)
            ("S qs=16 U",   nToM_Unbounded 16 mkSmallMessage)
          ]
    ]

mkBigMessage :: Int -> BigMessage
mkBigMessage !i =
  MkBigMessage
    ( "The not so very very very very very very very very very very very very very very very very very very very very very very very very " ++ show i,
      "large",
      "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeessssssssssssssssssssssssssssssssss" ++ show i,
      ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
        even i,
        123423421111111111111111111123234 * toInteger i
      )
    )

newtype BigMessage = MkBigMessage ([Char], [Char], [Char], ([Char], [Char], Bool, Integer))

mkSmallMessage :: Int -> SmallMessage
mkSmallMessage !i =
  MkSmallMessage
    (show i)

newtype SmallMessage = MkSmallMessage [Char]


nToM_M2 ::
  MonadUnliftIO m =>  Int -> (Int -> a) -> (Int, Int, Int) -> m ()
nToM_M2 !queueSize msgGen (!nSenders, !nMsgsTotal, !nReceivers) = do
  (receiverThreads, receiverOutBoxes) <- startReceivers
  let senderThreads = startSenders receiverOutBoxes
  runConc (senderThreads <> receiverThreads)
  where
    nMsgsPerReceiver = nMsgsTotal `div` nReceivers
    nMsgsPerSender = nMsgsPerReceiver `div` nSenders
    startSenders !receiverOutBoxes =
      stimes nSenders (conc senderLoop)
      where
        senderLoop =
          mapM_
            sendM2
            ((,) <$> receiverOutBoxes <*> (msgGen <$> [0 .. nMsgsPerSender - 1]))


        {-# INLINE sendM2 #-}
        sendM2 (!o, !a) =
          M2.trySendAndWait 5_000_000 o a
            >>= \case
              False -> error (show "M2.trySendAndWait timed out")
              True -> return ()

    startReceivers = do
      inBoxes <- replicateM nReceivers (M2.createInBox queueSize)
      let receivers = foldMap (conc . receiverLoop nMsgsPerReceiver) inBoxes
      outBoxes <- traverse M2.createOutBoxForInbox inBoxes
      return (receivers, outBoxes)
      where
        receiverLoop workLeft inBox
          | workLeft < 1 = pure ()
          | otherwise = do
            !_msg <- M2.receive inBox
            receiverLoop (workLeft - 1) inBox



UnbToM_Unbounded ::
  MonadUnliftIO m =>  Int -> (Int -> a) -> (Int, Int, Int) -> m ()
UnbToM_Bounded !queueSize msgGen (!nSenders, !nMsgsTotal, !nReceivers) = do
  (receiverThreads, receiverOutBoxes) <- startReceivers
  let senderThreads = startSenders receiverOutBoxes
  runConc (senderThreads <> receiverThreads)
  where
    nMsgsPerReceiver = nMsgsTotal `div` nReceivers
    nMsgsPerSender = nMsgsPerReceiver `div` nSenders
    startSenders !receiverOutBoxes =
      stimes nSenders (conc senderLoop)
      where
        senderLoop =
          mapM_
            UnbendBounded
            ((,) <$> receiverOutBoxes <*> (msgGen <$> [0 .. nMsgsPerSender - 1]))


        {-# INLINE UnbendBounded #-}
        UnbendBounded (!o, !a) =
          Unbounded.trySendAndWait 5_000_000 o a
            >>= \case
              False -> error (show "Unbounded.trySendAndWait timed out")
              True -> return ()

    startReceivers = do
      inBoxes <- replicateM nReceivers (Unbounded.createInBox queueSize)
      let receivers = foldMap (conc . receiverLoop nMsgsPerReceiver) inBoxes
      outBoxes <- traverse Unbounded.createOutBoxForInbox inBoxes
      return (receivers, outBoxes)
      where
        receiverLoop workLeft inBox
          | workLeft < 1 = pure ()
          | otherwise = do
            !_msg <- Unbounded.receive inBox
            receiverLoop (workLeft - 1) inBox
