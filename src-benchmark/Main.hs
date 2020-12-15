{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad (replicateM)
import Criterion.Main (defaultMain)
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Semigroup (Semigroup (stimes))
import qualified Protocol.MessageBox as Bounded
import qualified Protocol.UnboundedMessageBox as Unbounded
import UnliftIO (MonadUnliftIO, conc, runConc)

main =
  defaultMain
    [ bgroup
        "unidirectionalMessagePassing"
        [ bench
            ( show mboxImplTitle <> " "
                <> show noMessages
                <> " "
                <> show senderNo
                <> "->"
                <> show receiverNo
            )
            ( nfAppIO
                impl
                (senderNo, noMessages, receiverNo)
            )
          | noMessages <- [1_000_00],
            (mboxImplTitle, impl) <-
              [ let x = B 16 in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = U in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = B 4096 in (show x, unidirectionalMessagePassing mkTestMessage x)
              ],
            (senderNo, receiverNo) <-
              [  (1, 1000),
                --(10, 100),
                --(1, 2),
                 (1, 1),
                --(2, 1),
                -- (100, 10),
                (1000, 1)
              ]
        ]
    ]

mkTestMessage :: Int -> TestMessage
mkTestMessage !i =
  MkTestMessage
    ( "The not so very very very very very very very very very very very very very very very very very very very very very very very very " ++ show i,
      "large",
      "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeessssssssssssssssssssssssssssssssss" ++ show i,
      ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
        even i,
        123423421111111111111111111123234 * toInteger i
      )
    )

newtype TestMessage = MkTestMessage ([Char], [Char], [Char], ([Char], [Char], Bool, Integer)) 
  deriving newtype Show

class MBox inbox outbox | inbox -> outbox, outbox -> inbox where
  data MBoxArg inbox
  
  newIBox :: MonadUnliftIO m => MBoxArg inbox -> m (inbox a)
  newOBox :: MonadUnliftIO m => inbox a -> m (outbox a)
  deliver :: MonadUnliftIO m => outbox a -> a -> m ()
  receive :: MonadUnliftIO m => inbox a -> m a

instance MBox Unbounded.InBox Unbounded.OutBox where
  data MBoxArg Unbounded.InBox = U
    deriving stock Show
  {-# INLINE newIBox #-}  
  newIBox _ = Unbounded.createInBox
  {-# INLINE newOBox #-}
  newOBox = Unbounded.createOutBoxForInbox
  {-# INLINE deliver #-}
  deliver !o !m = Unbounded.deliver o m
  {-# INLINE receive #-}
  receive = Unbounded.receive

instance MBox Bounded.InBox Bounded.OutBox where
  data MBoxArg Bounded.InBox = B Int
    deriving stock Show
  {-# INLINE newIBox #-}    
  newIBox (B !limit) = Bounded.createInBox limit
  {-# INLINE newOBox #-}
  newOBox = Bounded.createOutBoxForInbox
  {-# INLINE deliver #-}
  deliver !o !a = Bounded.deliver o a
  {-# INLINE receive #-}
  receive = Bounded.receive

unidirectionalMessagePassing ::
  (MonadUnliftIO m, MBox inbox outbox) =>
  (Int -> TestMessage) ->
  MBoxArg inbox ->
  (Int, Int, Int) ->
  m ()
unidirectionalMessagePassing !msgGen !impl (!nSenders, !nMsgsTotal, !nReceivers) = do
  (receiverThreads, receiverOutBoxes) <- mkReceivers
  let senderThreads = mkSenders receiverOutBoxes
  runConc (senderThreads <> receiverThreads)
  where
    nMsgsPerReceiver = nMsgsTotal `div` nReceivers
    nMsgsPerSender = nMsgsPerReceiver `div` nSenders
    mkSenders !receiverOutBoxes =
      stimes nSenders (conc senderLoop)
      where
        senderLoop =
          mapM_
            (uncurry (flip deliver))
            ((,) <$> (msgGen <$> [0 .. nMsgsPerSender - 1]) <*> receiverOutBoxes)
    mkReceivers = do
      inBoxes <- replicateM nReceivers (newIBox impl)
      let receivers = foldMap (conc . receiverLoop nMsgsPerReceiver) inBoxes
      outBoxes <- traverse newOBox inBoxes
      return (receivers, outBoxes)
      where
        receiverLoop workLeft inBox
          | workLeft < 1 = pure ()
          | otherwise = do
            !_msg <- receive inBox
            receiverLoop (workLeft - 1) inBox
