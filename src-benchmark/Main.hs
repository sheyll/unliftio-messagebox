{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
import qualified Data.IntMap as Map
import Data.Map (Map)
import Data.Semigroup (Semigroup (stimes))
import Data.Set (Set)
import Protocol.BoundedMessageBox (InBoxConfig (BoundedMessageBox))
import Protocol.Command as Command
import Protocol.Fresh
import Protocol.MessageBoxClass (IsMessageBox (..), deliver, receive)
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import RIO
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
          | noMessages <- [100_000],
            (mboxImplTitle, impl) <-
              [ let x = BoundedMessageBox 16 in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = UnboundedMessageBox in (show x, unidirectionalMessagePassing mkTestMessage x),
                let x = BoundedMessageBox 4096 in (show x, unidirectionalMessagePassing mkTestMessage x)
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

unidirectionalMessagePassing ::
  (MonadUnliftIO m, IsMessageBox inbox outbox) =>
  (Int -> TestMessage) ->
  InBoxConfig inbox ->
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
          mapM_
            (uncurry (flip deliver))
            ((,) <$> (msgGen <$> [0 .. (nM `div` (nC * nP)) - 1]) <*> cs)
    consumers = do
      cis <- replicateM nC (newInBox impl)
      let ccs = foldMap (conc . consume (nM `div` nC)) cis
      cs <- traverse newOutBox cis
      return (ccs, cs)
      where
        consume 0 _inBox = return ()
        consume workLeft inBox = do
          !_msg <- receive inBox
          consume (workLeft - 1) inBox

-- Command Benchmark

data MediaApi

data MediaClientApi

data MixingTreeApi

type DspId = Int

type MixerId = Int

type MemberId = Int

type ConferenceId = Int

data instance Command MediaApi r where
  FetchDsps :: Command MediaApi ( 'Return (Set DspId))
  CreateMixer :: DspId -> Command MediaApi ( 'Return (Maybe MixerId))
  DestroyMixer :: MixerId -> Command MediaApi 'FireAndForget
  AddToMixer :: DspId -> MixerId -> MemberId -> Command MediaApi ( 'Return Bool)
  RemoveFromMixer :: DspId -> MixerId -> MemberId -> Command MediaApi ( 'Return ())

data instance Command MediaClientApi r where
  OnCallConnected :: ConferenceId -> DspId -> MemberId -> Command MediaClientApi 'FireAndForget
  OnCallDisconnected :: ConferenceId -> DspId -> MemberId -> Command MediaClientApi 'FireAndForget
  MemberJoined :: ConferenceId -> MemberId -> Command MediaApi 'FireAndForget
  MemberUnJoined :: ConferenceId -> MemberId -> Command MediaApi 'FireAndForget

data instance Command MixingTreeApi r where
  CreateConference :: ConferenceId -> Command MixingTreeApi ( 'Return ())
  DestroyConference :: ConferenceId -> Command MixingTreeApi ( 'Return ())
  Join :: ConferenceId -> MemberId -> Command MixingTreeApi 'FireAndForget
  UnJoin :: ConferenceId -> MemberId -> Command MixingTreeApi 'FireAndForget

type Capacity = Int

newtype MemberState = MemberState {isMemberJoined :: Bool}

data MediaEnv = MediaEnv
  { dsps :: IORef (Map DspId Capacity),
    mixerIdCounter :: CounterVar MixerId
  }

instance HasCounterVar MixerId MediaEnv where
  getCounterVar = mixerIdCounter

runMediaEnv :: Message MediaApi r -> RIO MediaEnv ()
runMediaEnv =
  \case
    Blocking FetchDsps replyBox -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let goodDsps = filter (>= 2) (Map.keys allDsps)
      replyTo replyBox goodDsps
    Blocking (CreateMixer dspId) replyBox -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let capacities = Map.lookup dspId
      case capacities of
        Just capacity | capacity > 0 -> do
          newMixerId <- fresh
          writeIORef'
            dspsVar
            (Map.insert dspId (capacity - 1) allDsps)
        _ ->
          replyTo replyBox Nothing
    Blocking (DestroyMixer dspId) -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let capacities = Map.lookup dspId
      traverse
        ( \capacity ->
            do
              newMixerId <- fresh
              writeIORef'
                dspsVar
                (Map.insert dspId (capacity + 1) allDsps)
        )
        capacities

data AppEnv = AppEnv
  { conferences :: Map ConferenceId (Map MemberId MemberState)
  }

data ConfEnv = ConfEnv
  { mixers :: Map (DspId, MixerId) (Set MemberId)
  }
