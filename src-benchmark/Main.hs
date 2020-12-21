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
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup (stimes))
import Data.Set (Set)
import Protocol.BoundedMessageBox (InBoxConfig (BoundedMessageBox))
import Protocol.Command as Command
import Protocol.Fresh
import Protocol.MessageBoxClass (IsMessageBox (..), deliver, receive)
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import RIO
import UnliftIO (MonadUnliftIO, conc, runConc)
import qualified Data.Set as Set

main =
  defaultMain
    [ bgroup "rcpMessagePassing"
        [ bench 
            ("manyFetchDspsDirectly " ++ show cfg) 
            (nfAppIO manyFetchDspsDirectly cfg) 
        | cfg <- [(100000,1),(100000, 1000)]]

    , bgroup
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


--------------------------------------------------------------------------------
-- Command Benchmarks

manyFetchDspsDirectly :: (Int,Int) -> IO ()
manyFetchDspsDirectly (nFetchesTotal, nClients) =
  error "TODO"
  

-- internals


data MediaApi

data MediaClientApi

data MixingTreeApi

type DspId = Int

type MixerId = Int

type MediaStreamId = Int

type MixingGroupId = Int

data instance Command MediaApi _ where
  FetchDsps :: Command MediaApi ( 'Return (Set DspId))
  CreateMixer :: DspId -> Command MediaApi ( 'Return (Maybe MixerId))
  DestroyMixer :: MixerId -> Command MediaApi 'FireAndForget
  AddToMixer :: DspId -> MixerId -> MediaStreamId -> Command MediaApi ( 'Return Bool)
  RemoveFromMixer :: DspId -> MixerId -> MediaStreamId -> Command MediaApi ( 'Return ())

data instance Command MediaClientApi _ where
  OnCallConnected :: MixingGroupId -> DspId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  OnCallDisconnected :: MixingGroupId -> DspId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  MemberJoined :: MixingGroupId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  MemberUnJoined :: MixingGroupId -> MediaStreamId -> Command MediaClientApi 'FireAndForget

data instance Command MixingTreeApi _ where
  CreateMixingGroup :: MixingGroupId -> Command MixingTreeApi ( 'Return ())
  DestroyMixingGroup :: MixingGroupId -> Command MixingTreeApi ( 'Return ())
  Join :: MixingGroupId -> MediaStreamId -> Command MixingTreeApi 'FireAndForget
  UnJoin :: MixingGroupId -> MediaStreamId -> Command MixingTreeApi 'FireAndForget

type Capacity = Int

newtype MemberState = MemberState {isMemberJoined :: Bool}

data MediaEnv = MediaEnv
  { dsps :: IORef (Map DspId Capacity),
    mixerIdCounter :: CounterVar MixerId
  }

instance HasCounterVar MixerId MediaEnv where
  getCounterVar = mixerIdCounter

runMediaEnv :: Message MediaApi -> RIO MediaEnv ()
runMediaEnv =
  \case
    Blocking FetchDsps replyBox -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let goodDsps = Set.filter (>= 2) (Map.keysSet allDsps)
      replyTo replyBox goodDsps
    Blocking (CreateMixer dspId) replyBox -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let capacities = Map.lookup dspId allDsps
      r <- case capacities of
        Just capacity | capacity > 0 -> do
          writeIORef
            dspsVar
            (Map.insert dspId (capacity - 1) allDsps)
          Just <$> fresh
        _ ->
          pure Nothing
      replyTo replyBox r    
    Blocking (AddToMixer _ _ _) replyBox -> do
      replyTo replyBox (error "TODO")    
    Blocking (RemoveFromMixer _ _ _) replyBox -> do
      replyTo replyBox (error "TODO")    
    NonBlocking (DestroyMixer dspId) -> do
      dspsVar <- asks dsps
      allDsps <- readIORef dspsVar
      let capacities = Map.lookup dspId allDsps
      traverse_
        ( \capacity ->
              writeIORef
                dspsVar
                (Map.insert dspId (max 0 (capacity - 1)) allDsps)
        )
        capacities

data AppEnv = AppEnv
  { mixingGroups :: Map MixingGroupId (Map MediaStreamId MemberState)
  }

data ConfEnv = ConfEnv
  { mixers :: Map (DspId, MixerId) (Set MediaStreamId)
  }
