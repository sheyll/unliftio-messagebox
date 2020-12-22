{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | More complex benchmarks, mimiking real world applications
-- from the media domain.
module MediaBenchmark (benchmark) where

import Control.Monad
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import qualified Data.Map.Strict as Map
import Data.Semigroup (Semigroup (stimes))
import qualified Data.Set as Set
import Protocol.Command as Command
  ( CallId,
    Command,
    Message (..),
    ReturnType (FireAndForget, Return),
    call,
    handleMessage,
    replyTo,
  )
import Protocol.Fresh
  ( CounterVar,
    HasCounterVar (getCounterVar),
    fresh,
    newCounterVar,
  )
import Protocol.MessageBoxClass (IsMessageBox (..))
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import qualified Protocol.UnboundedMessageBox as Unbounded
import RIO
  ( Conc,
    IORef,
    Map,
    RIO,
    Set,
    asks,
    conc,
    readIORef,
    runConc,
    runRIO,
    traverse_,
    writeIORef,
  )

benchmark =
  bgroup
    "Media"
    [ bench
        ( "fetchDsps: "
            ++ show nF
            ++ " total fetches "
            ++ show nC
            ++ " clients"
        )
        (nfAppIO fetchDspsBench cfg)
      | cfg@(nF, nC) <-
          [(100000, 1), (100000, 1000)]
    ]

-- | Run @nClients@ processes that each call 'FetchDsps' so many times
-- that globally @nFetchesTotal@ times 'FetchDsps' is called.
fetchDspsBench :: (Int, Int) -> IO ()
fetchDspsBench (nFetchesTotal, nClients) =
  let startClients serverOut =
        stimes nClients (conc (client perClientWork))
        where
          client 0 = return ()
          client !workLeft =
            call serverOut FetchDsps 5_000_000
              >>= either
                (error . show)
                (const (client (workLeft - 1)))

      startServer ::
        RIO
          (CounterVar CallId)
          ( Unbounded.OutBox (Message MediaApi),
            Conc (RIO (CounterVar CallId)) ()
          )
      startServer = do
        serverIn <- newInBox UnboundedMessageBox
        serverOut <- newOutBox serverIn
        let dspSet = Set.fromList [0 .. 4096]
        return (serverOut, conc (server serverWork serverIn dspSet))
        where
          server ::
            Int ->
            Unbounded.InBox (Message MediaApi) ->
            Set DspId ->
            RIO (CounterVar CallId) ()
          server 0 _ _ = return ()
          server !workLeft !serverIn !dspSet =
            handleMessage
              serverIn
              ( \case
                  Blocking FetchDsps replyBox -> do
                    replyTo replyBox dspSet
                    server (workLeft - 1) serverIn dspSet
                  Blocking other _ ->
                    error ("unexpected command: " ++ show other)
                  NonBlocking other ->
                    error ("unexpected command: " ++ show other)
              )
              >>= maybe (error "handleMessage failed") return

      perClientWork = nFetchesTotal `div` nClients
      serverWork = perClientWork * nClients -- not the same as nFetchesTotal
   in do
        callIdCounter <- newCounterVar
        runRIO callIdCounter $ do
          (serverOut, serverConc) <- startServer
          runConc (serverConc <> startClients serverOut)

-- | A benchmark on a simulated application workload.
--
-- The applicaton has three layers:
--
-- 1. Media
--    * Per dsp MediaApi processes
--    * A dispatcher
-- 2. Media Stream Grouping
--    * A process per media stream group
-- 3. Client
--    * A process that processes a benchmark specific set of
--      random requests for grouping media
--
-- Startup and shutdown:
--  When a DSP is full, the DSP process will exit.
--  

mediaAppBenchmark ::
  Map DspId Capacity ->
  Map MixingGroupId (DspId, Set MediaStreamId) ->
  IO ()
mediaAppBenchmark !availableDsps !testMixingGroups = do
  (mediaOutBox, mediaConc) <- spawnMediaApi
  error "TODO"
  where
    spawnMediaApi = do
      (perDspOutBoxes, perDspConc) <- dspMediaApiProcesses
      spawnMediaProxy perDspOutBoxes
      where
        spawnMediaProxy perDspOutBoxes = do
          error "TODO"
        dspMediaApiProcesses =
          fmap
            unzip
            ( forM
                (Map.toList availableDsps)
                ( \(myDspId, myCapa) -> do
                    return (myOutBox, (myDspId, conc))
                )
            )

-- * Types for the domain of the benchmarks in this module

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

deriving stock instance Show (Command MediaApi ( 'Return (Set DspId)))

deriving stock instance Show (Command MediaApi ( 'Return (Maybe MixerId)))

deriving stock instance Show (Command MediaApi ( 'Return Bool))

deriving stock instance Show (Command MediaApi ( 'Return ()))

deriving stock instance Show (Command MediaApi 'FireAndForget)

data instance Command MediaClientApi _ where
  OnMediaStreamCreated :: MixingGroupId -> DspId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  OnMediaStreamDestroyed :: MixingGroupId -> DspId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  MemberJoined :: MixingGroupId -> MediaStreamId -> Command MediaClientApi 'FireAndForget
  MemberUnJoined :: MixingGroupId -> MediaStreamId -> Command MediaClientApi 'FireAndForget

data instance Command MixingTreeApi _ where
  CreateMixingGroup :: MixingGroupId -> Command MixingTreeApi ( 'Return ())
  DestroyMixingGroup :: MixingGroupId -> Command MixingTreeApi ( 'Return ())
  Join :: MixingGroupId -> MediaStreamId -> Command MixingTreeApi 'FireAndForget
  UnJoin :: MixingGroupId -> MediaStreamId -> Command MixingTreeApi 'FireAndForget

type Capacity = Int

newtype MemberState = MemberState {isMemberJoined :: Bool}

-- | General purpose 'MediaApi' server
-- Run a MediaApi message handler, that exists when no message was received for
-- more than one second.
mediaApiSim ::
  (HasCounterVar MixerId env) =>
  Map DspId Capacity ->
  MixerId ->
  RIO env ()
mediaApiSim myDsps nextMixerId = error "TODO"


-- Core of the media simulation: Handle MediaApi requests and manage a map of dsps.
mediaSimHandleMessage ::
  Map DspId Capacity ->
  Message MediaApi ->
  RIO  (Map DspId Capacity)
mediaSimHandleMessage allDsps =
  \case
    Blocking FetchDsps replyBox -> do
      let goodDsps = Set.filter (>= 2) (Map.keysSet allDsps)
      replyTo replyBox goodDsps
      return allDsps
    Blocking (CreateMixer dspId) replyBox -> 
      case Map.lookup dspId allDsps of
        Just capacity | capacity > 0 -> do                
            fresh >>= replyTo replyBox . Just 
            return (Map.insert dspId (capacity - 1) allDsps)
        _ -> do
            replyTo replyBox Nothing 
            return allDsps
    Blocking (AddToMixer _dspId _ _) _replyBox -> do
      error "TODO"
    Blocking (RemoveFromMixer _dspId _ _) _replyBox -> 
      error "TODO"
    NonBlocking (DestroyMixer dspId) -> do
      case Map.lookup dspId allDsps of
        Just capacity | capacity > 0 -> do                
            return (Map.insert dspId (capacity + 1) allDsps)
        _ -> do
            return allDsps
