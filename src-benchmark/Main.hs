{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
            ( mboxImplTitle <> " "
                <> show noMessages
                <> " "
                <> show senderNo
                <> " >>= "
                <> show receiverNo
            )
            ( nfAppIO
                impl
                (senderNo, noMessages, receiverNo)
            )
          | noMessages <- [1_000_000],
            (mboxImplTitle, impl) <-
              [ let x = B 16 in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = U in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = B 4096 in (show x, unidirectionalMessagePassing mkTestMessage x)
              ],
            (senderNo, receiverNo) <-
              [ (1, 1000),
                (10, 100),
                (1, 1),
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
  deriving newtype (Show)

class MBox inbox outbox | inbox -> outbox, outbox -> inbox where
  data MBoxArg inbox

  newIBox :: MonadUnliftIO m => MBoxArg inbox -> m (inbox a)
  newOBox :: MonadUnliftIO m => inbox a -> m (outbox a)
  deliver :: MonadUnliftIO m => outbox a -> a -> m ()
  receive :: MonadUnliftIO m => inbox a -> m a

instance MBox Unbounded.InBox Unbounded.OutBox where
  data MBoxArg Unbounded.InBox = U
    deriving stock (Show)
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
    deriving stock (Show)
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
unidirectionalMessagePassing !msgGen !impl (!nP, !nM, !nC) = do
  (ccs, cs) <- consumers
  let ps = producers cs
  runConc (ps <> ccs)
  where
    producers !cs = stimes nP (conc producer)
      where
        producer = 
          mapM_ (uncurry (flip deliver))
                ((,) <$> (msgGen <$> [0 .. (nM `div` (nC * nP)) - 1]) <*> cs)
    consumers = do
      cis <- replicateM nC (newIBox impl)
      let ccs = foldMap (conc . consume (nM `div` nC)) cis
      cs <- traverse newOBox cis
      return (ccs, cs)
      where
        consume 0 _inBox = return ()
        consume workLeft inBox = do
            !_msg <- receive inBox
            consume (workLeft - 1) inBox
