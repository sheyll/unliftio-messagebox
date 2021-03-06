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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | More complex benchmarks, mimiking real world applications
-- from the media domain.
module MediaBenchmark (mediaAppBenchmark, Param (..)) where

import Control.Monad (when)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    ReaderT (runReaderT),
    asks,
    fix,
    forM,
    forM_,
    replicateM_,
    unless,
    void,
  )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import GHC.Stats
import UnliftIO
  ( Conc,
    SomeException,
    TVar,
    atomically,
    checkSTM,
    conc,
    newTVarIO,
    readTVar,
    runConc,
    try,
    writeTVar,
  )
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.MessageBox

-- | A benchmark of a fictional media mixing application.
--
-- The applicaton has these layers:
--
-- 1. Media resources (DSPs) for mixing media streams (aka members)
--    * One MediaApi process with @i@ Dsps
--    * Exits after a stop message is received
--      and all media resources are removed.
--
-- 2. Logical Mixing Groups
--    * one process per group
--    * use the media resources to mix groups
--      spanning several DSPs
--    * create bridges between low-level mixers
--
-- 3. Mixing Group Broker
--    * Starts/Stops the processes for the
--      logical mixing groups.
--    * Delegates the MixingApi to the
--      logical mixing group processes
--    * When all groups are destroyed:
--       - Send the media resources process a stop message
--       - exit
--
-- 4. Mixing Group Applications (aka clients)
--    * uses the Mixing Group Broker
--    * corresponds to a single exclusive mixing group
--    * Follows this procedure:
--      - create the mixing group
--      - add members
--      - remove members
--      - destroy the group
--      - exit
--
-- This whole process is repeated @m@ times.
--
-- When all clients have finished, send a shutdown message to the media process,
-- and wait for every process to finish.
mediaAppBenchmark ::
  forall cfg.
  (HasCallStack, IsMessageBoxArg cfg) =>
  cfg ->
  Param ->
  IO ()
mediaAppBenchmark cfg param = do
  currentIterationVar <- newTVarIO 0
  (mixingOut, c1) <- do
    (mediaInput, mediaConc) <- spawnMediaApi
    (mixingOut, mixingConc) <- spawnMixingBroker currentIterationVar mediaInput
    return (mixingOut, mediaConc <> mixingConc)
  appCounters <- AppCounters <$> newCounterVar <*> newCounterVar
  let clients = spawnMixingApps currentIterationVar mixingOut
  runReaderT (runConc (c1 <> clients)) appCounters
  where
    spawnMediaApi = do
      mediaOutput <- newMessageBox cfg
      mediaInput <- newInput mediaOutput
      let startMediaServer = do
            liftIO $ putStrLn "===== BEGIN: Media Server Loop"
            forM_ [1 .. nRounds param] $ \iteration -> do
              liftIO $ putStrLn ("===== ITERATION: " ++ show iteration ++ ": Media Server Loop ")
              mediaServerLoop (mkMediaSimSt (toDspConfig param))
              liftIO $ do
                hasStats <- getRTSStatsEnabled
                when hasStats $ do
                  rtsStats <- getRTSStats
                  putStrLn
                    ( "Total memory in use: "
                        ++ show (gcdetails_mem_in_use_bytes (gc rtsStats) `div` (1024 * 1024))
                        ++ "m"
                    )
                putStrLn ("===== FINISHED: " ++ show iteration ++ ": Media Server Loop")
            liftIO $ putStrLn "===== END: Media Server Loop"
          mediaServerLoop st = do
            let !isFinished =
                  shuttingDown st && Map.null (allMixers st)
            unless isFinished $
              try
                (handleMessage mediaOutput (handleMediaApi st))
                >>= either
                  ( liftIO
                      . putStrLn
                      . ("media server failed to receive next message: " ++)
                      . (show :: SomeException -> String)
                  )
                  (maybe (error "media server loop premature exit") mediaServerLoop)
      return (mediaInput, conc startMediaServer)

    spawnMixingBroker ::
      TVar Int ->
      Input (MessageBox cfg) (Message MediaApi) ->
      IO (Input (MessageBox cfg) (Message MixingApi), Conc (ReaderT AppCounters IO) ())
    spawnMixingBroker currentIterationVar mediaBoxOut = do
      mixingOutput <- newMessageBox cfg
      mixingInput <- newInput mixingOutput
      let startMixingServer = do
            let groupMap :: Map MixingGroupId (Input (MessageBox cfg) (Message MixingApi))
                groupMap = Map.empty
            liftIO $ putStrLn "===== BEGIN: Mixing Server Loop"
            forM_ [1 .. nRounds param] $ \iteration -> do
              liftIO $ putStrLn ("===== ITERATION: " ++ show iteration ++ ": Mixing Server Loop ")
              atomically $ writeTVar currentIterationVar iteration
              mixingServerLoop iteration (0, groupMap)
              liftIO $ putStrLn ("===== FINISHED: " ++ show iteration ++ ": Mixing Server Loop")
              when (iteration < nRounds param) (threadDelay 500_000)
            liftIO $ putStrLn "===== END: Mixing Server Loop"

          mixingServerLoop !iteration !groupMap =
            try
              ( handleMessage
                  mixingOutput
                  (dispatchMixingApi groupMap)
              )
              >>= either
                ( liftIO
                    . putStrLn
                    . ("mixingServerLoop failed: " ++)
                    . (show :: SomeException -> String)
                )
                ( maybe
                    (error "mixing server loop premature exit")
                    ( \(!nDestroyed', !groupMap') ->
                        if Map.null groupMap' && nDestroyed' == nGroups param
                          then void $ cast mediaBoxOut MediaShutdown
                          else mixingServerLoop iteration (nDestroyed', groupMap')
                    )
                )
          dispatchMixingApi ::
            (Int, Map MixingGroupId (Input (MessageBox cfg) (Message MixingApi))) ->
            Message MixingApi ->
            ReaderT
              AppCounters
              IO
              (Int, Map MixingGroupId (Input (MessageBox cfg) (Message MixingApi)))
          dispatchMixingApi (!nDestroyed, !st) =
            \case
              Blocking cm@(CreateMixingGroup !mgId) !r -> do
                unless
                  (Map.notMember mgId st)
                  (error ("Mixing group ID conflict: " ++ show mgId))
                !mgInput <- spawnMixingGroup mediaBoxOut
                !ok <- delegateCall mgInput cm r
                unless ok (error ("delegation failed: " ++ show cm))
                return (nDestroyed, Map.insert mgId mgInput st)
              Blocking (DestroyMixingGroup !mgId) !r ->
                case Map.lookup mgId st of
                  Nothing ->
                    error ("DestroyMixingGroup: Mixing group doesn't exist: " ++ show mgId)
                  Just !mgInput -> do
                    !ok <- delegateCall mgInput (DestroyMixingGroup mgId) r
                    unless ok (error ("delegation failed: " ++ show (DestroyMixingGroup mgId)))
                    return (nDestroyed + 1, Map.delete mgId st)
              NonBlocking m@(Join !mgId _ _) ->
                case Map.lookup mgId st of
                  Nothing ->
                    error ("Mixing group doesn't exist: " ++ show mgId ++ " in: " ++ show m)
                  Just !mgInput -> do
                    !ok <- cast mgInput m
                    unless ok (error ("delegation failed: " ++ show m))
                    return (nDestroyed, st)
              NonBlocking m@(UnJoin !mgId _ _) ->
                case Map.lookup mgId st of
                  Nothing ->
                    error ("Mixing group doesn't exist: " ++ show mgId ++ " in: " ++ show m)
                  Just !mgInput -> do
                    !ok <- cast mgInput m
                    unless ok (error ("delegation failed: " ++ show m))
                    return (nDestroyed, st)
      return (mixingInput, conc startMixingServer)
    spawnMixingGroup ::
      Input (MessageBox cfg) (Message MediaApi) ->
      ReaderT AppCounters IO (Input (MessageBox cfg) (Message MixingApi))
    spawnMixingGroup !mediaInput = do
      !mgOutput <- newMessageBox cfg
      !mgInput <- newInput mgOutput
      let mgLoop (!mgId, !groupMap) =
            try
              ( handleMessage
                  mgOutput
                  (handleCmd (mgId, groupMap))
              )
              >>= either
                ( liftIO
                    . putStrLn
                    . (("mixingGroup " ++ show mgId ++ " exception: ") ++)
                    . (show :: SomeException -> String)
                )
                ( maybe
                    (error ("mixingGroup loop " ++ show mgId ++ " premature exit"))
                    (maybe (return ()) mgLoop)
                )
          handleCmd ::
            (MixingGroupId, Map DspId (MixerId, Set MemberId)) ->
            Message MixingApi ->
            ReaderT AppCounters IO (Maybe (MixingGroupId, Map DspId (MixerId, Set MemberId)))
          handleCmd (!mgId, !st) =
            \case
              Blocking (CreateMixingGroup !mgId') !r -> do
                replyTo r ()
                return (Just (mgId', st))
              Blocking (DestroyMixingGroup _) !r -> do
                replyTo r ()
                return Nothing -- exit
              NonBlocking (Join !_mgId !memberId !callBack) ->
                call mediaInput FetchDsps 500_000
                  >>= either
                    ( \ !mErr -> do
                        void $ deliver callBack (MemberUnJoined mgId memberId)
                        error (show mErr)
                    )
                    ( \ !dsps ->
                        let selectedDspId =
                              let !ks = Set.toList dsps
                                  !nDsps = length ks
                                  !ki = memberId `rem` nDsps
                               in ks !! ki
                            doAdd !theMixerId !theMembers =
                              let !m = AddToMixer theMixerId memberId
                               in call mediaInput m 500_000
                                    >>= \case
                                      Left !err ->
                                        error (show m ++ " failed: " ++ show err)
                                      Right False -> do
                                        void $ deliver callBack (MemberUnJoined mgId memberId)
                                        error (show m ++ " did not succeed")
                                      Right True -> do
                                        let st' =
                                              Map.insert
                                                selectedDspId
                                                (theMixerId, Set.insert memberId theMembers)
                                                st
                                        void $ deliver callBack (MemberJoined mgId memberId)
                                        return (Just (mgId, st'))
                         in if Set.null dsps
                              then error "Not enough DSP capacity"
                              else case Map.lookup selectedDspId st of
                                Nothing ->
                                  call mediaInput (CreateMixer selectedDspId) 500_000
                                    >>= \case
                                      Left !err ->
                                        error (show err)
                                      Right Nothing ->
                                        error ("create mixer failed on: " ++ show selectedDspId)
                                      Right (Just !theMixerId) ->
                                        doAdd theMixerId Set.empty
                                Just (!mixerId, !members) -> doAdd mixerId members
                    )
              NonBlocking (UnJoin _ !memberId !callBack) ->
                case Map.toList (Map.filter (Set.member memberId . snd) st) of
                  ((!theDspId, (!theMixerId, !theMembers)) : _) -> do
                    call mediaInput (RemoveFromMixer theMixerId memberId) 500_000
                      >>= either (error . show) (const (return ()))
                    let theMembers' = Set.delete memberId theMembers
                    if Set.null theMembers'
                      then do
                        !ok <- cast mediaInput (DestroyMixer theMixerId)
                        unless ok (error (show (DestroyMixer theMixerId) ++ " failed!"))
                        void $ deliver callBack (MemberUnJoined mgId memberId)
                        return (Just (mgId, Map.delete theDspId st))
                      else do
                        void $ deliver callBack (MemberUnJoined mgId memberId)
                        return (Just (mgId, Map.insert theDspId (theMixerId, theMembers') st))
                  [] ->
                    return (Just (mgId, st))
      void $ forkIO (mgLoop (-1, Map.empty))
      return mgInput

    spawnMixingApps currentIterationVar mixingInput =
      let !clients = foldMap spawnClient [0 .. nGroups param - 1]
          spawnClient !mixingGroupId = conc $ do
            let isLogged = mixingGroupId == nGroups param - 1
            when
              isLogged
              (liftIO $ putStrLn ("Client: " ++ show mixingGroupId ++ " started."))
            forM_ [1 .. nRounds param] $ \iteration -> do
              atomically $ readTVar currentIterationVar >>= checkSTM . (== iteration)
              when
                isLogged
                (liftIO $ putStrLn ("Client: " ++ show mixingGroupId ++ " started iteration: " ++ show iteration))
              eventsIn <- newMessageBox cfg
              eventsOut <- newInput eventsIn
              -- create conference,
              call mixingInput (CreateMixingGroup mixingGroupId) 1_000_000
                >>= either
                  (error . ((show (CreateMixingGroup mixingGroupId) ++ " failed: ") ++) . show)
                  (const (return ()))
              -- add participants and wait for the joined event
              !members <- forM [0 .. nMembers param - 1] $ \i -> do
                let !memberId = nMembers param * mixingGroupId + i
                !castSuccessful <- cast mixingInput (Join mixingGroupId memberId eventsOut)
                unless
                  castSuccessful
                  (error ("Failed to cast: " ++ show (Join mixingGroupId memberId eventsOut)))
                return memberId
              replicateM_
                (nMembers param)
                ( fix $ \ ~again ->
                    receive eventsIn
                      >>= \case
                        Just (MemberJoined _ _) ->
                          return ()
                        Just unexpected ->
                          error ("Unexpected mixing group event: " ++ show unexpected ++ " expected MemberJoined")
                        Nothing ->
                          again
                )
              when
                isLogged
                (liftIO $ putStrLn ("Client: " ++ show mixingGroupId ++ " joined participants: " ++ show iteration))
              -- remove participants and wait for unjoined
              forM_ members $ \ !memberId -> do
                !castSuccessful <- cast mixingInput (UnJoin mixingGroupId memberId eventsOut)
                unless
                  castSuccessful
                  (error ("Failed to cast: " ++ show (UnJoin mixingGroupId memberId eventsOut)))
              replicateM_
                (nMembers param)
                ( fix $ \ ~again ->
                    receiveAfter eventsIn 500_000
                      >>= \case
                        Just (MemberUnJoined _ _) ->
                          return ()
                        Just unexpected ->
                          error ("Unexpected mixing group event: " ++ show unexpected ++ " expected MemberUnJoined")
                        Nothing ->
                          again
                )
              -- destroy the conference,
              call mixingInput (DestroyMixingGroup mixingGroupId) 500_000
                >>= either
                  (error . ((show (DestroyMixingGroup mixingGroupId) ++ " failed: ") ++) . show)
                  (const (return ()))

              when
                isLogged
                ( liftIO $
                    putStrLn
                      ( "Client: "
                          ++ show mixingGroupId
                          ++ " unjoined participants: "
                          ++ show iteration
                      )
                )
              when
                (iteration < nRounds param)
                ( do
                    when
                      isLogged
                      ( liftIO $
                          putStrLn
                            ( "Client: "
                                ++ show mixingGroupId
                                ++ " finished with iteration: "
                                ++ show iteration
                                ++ " sleeping before next iteration."
                            )
                      )
                )
       in clients

data AppCounters = AppCounters
  { callIdCounter :: !(CounterVar CallId),
    idCounter :: !(CounterVar Int)
  }

instance HasCallIdCounter AppCounters where
  getCallIdCounter = asks callIdCounter

instance HasCounterVar Int AppCounters where
  getCounterVar = asks idCounter

data Param = Param
  { nDsps :: !Int,
    nGroups :: !Int,
    nMembers :: !Int,
    nRounds :: !Int
  }

instance Show Param where
  showsPrec _ Param {nDsps, nGroups, nMembers} =
    shows nDsps . showString "DSPs/"
      . shows nGroups
      . showString "GRPs/"
      . shows nMembers
      . showString "MEMBERS"

toDspConfig :: Param -> Map DspId Capacity
toDspConfig p =
  let perDspCapacity =
        max 2 (2 * requiredTotal) -- (2 * (ceiling (fromIntegral requiredTotal / fromIntegral (nDsps p) :: Double)))
      requiredTotal = nGroups p + nGroups p * nMembers p
   in Map.fromList ([0 ..] `zip` replicate (nDsps p) perDspCapacity)

-- * Types for the domain of the benchmarks in this module

data MediaApi

data MixingApi

type DspId = Int

type MixerId = Int

type MemberId = Int

type MixingGroupId = Int

data instance Command MediaApi _ where
  FetchDsps :: Command MediaApi ('Return (Set DspId))
  CreateMixer :: DspId -> Command MediaApi ('Return (Maybe MixerId))
  DestroyMixer :: MixerId -> Command MediaApi 'FireAndForget
  AddToMixer :: MixerId -> MemberId -> Command MediaApi ('Return Bool)
  RemoveFromMixer :: MixerId -> MemberId -> Command MediaApi ('Return ())
  MediaShutdown :: Command MediaApi 'FireAndForget

deriving stock instance Show (Command MediaApi ('Return (Set DspId)))

deriving stock instance Show (Command MediaApi ('Return (Maybe MixerId)))

deriving stock instance Show (Command MediaApi ('Return Bool))

deriving stock instance Show (Command MediaApi ('Return ()))

deriving stock instance Show (Command MediaApi 'FireAndForget)

data MixingGroupEvent where
  MemberJoined :: MixingGroupId -> MemberId -> MixingGroupEvent
  MemberUnJoined :: MixingGroupId -> MemberId -> MixingGroupEvent
  deriving stock (Show, Eq)

data instance Command MixingApi _ where
  CreateMixingGroup ::
    MixingGroupId ->
    Command MixingApi ('Return ())
  DestroyMixingGroup ::
    MixingGroupId ->
    Command MixingApi ('Return ())
  Join ::
    IsInput outBox =>
    MixingGroupId ->
    MemberId ->
    outBox MixingGroupEvent ->
    Command MixingApi 'FireAndForget
  UnJoin ::
    IsInput outBox =>
    MixingGroupId ->
    MemberId ->
    outBox MixingGroupEvent ->
    Command MixingApi 'FireAndForget

instance Show (Command MixingApi ('Return ())) where
  showsPrec d (CreateMixingGroup i) =
    showParen (d >= 9) (showString "CreateMixingGroup " . shows i)
  showsPrec d (DestroyMixingGroup i) =
    showParen (d >= 9) (showString "DestroyMixingGroup " . shows i)

instance Show (Command MixingApi 'FireAndForget) where
  showsPrec d (Join i j _) =
    showParen (d >= 9) (showString "Join " . shows i . showChar ' ' . shows j)
  showsPrec d (UnJoin i j _) =
    showParen (d >= 9) (showString "UnJoin " . shows i . showChar ' ' . shows j)

type Capacity = Int

-- | Mix media streams using 'MediaApi'

-- | Media server simulation
--
-- Handle 'MediaApi' requests and manage a map of
-- dsps and mixers.
handleMediaApi ::
  MediaSimSt ->
  Message MediaApi ->
  ReaderT env IO MediaSimSt
handleMediaApi !st =
  \case
    Blocking FetchDsps replyBox -> do
      let !goodDsps = Map.keysSet (Map.filter (> 1) (allDsps st))
      replyTo replyBox goodDsps
      return st
    Blocking (CreateMixer dspId) replyBox ->
      case Map.lookup dspId (allDsps st) of
        Just capacity | capacity > 0 -> do
          let theNewMixerId = nextMixerId st
          replyTo replyBox (Just theNewMixerId)
          return
            ( st
                { allDsps =
                    Map.insert
                      dspId
                      (capacity - 1)
                      (allDsps st),
                  allMixers =
                    Map.insert
                      theNewMixerId
                      (dspId, Set.empty)
                      (allMixers st),
                  nextMixerId = theNewMixerId + 1
                }
            )
        _ -> do
          replyTo replyBox Nothing
          return st
    Blocking (AddToMixer theMixer newMember) replyBox ->
      case Map.lookup theMixer (allMixers st) of
        Nothing ->
          replyTo replyBox False >> return st
        Just (!dsp, !streams) ->
          if Set.member newMember streams
            then replyTo replyBox True >> return st
            else case Map.lookup dsp (allDsps st) of
              Just capacity
                | capacity >= 1 ->
                  replyTo replyBox True
                    >> return
                      ( st
                          { allMixers =
                              Map.insert
                                theMixer
                                (dsp, Set.insert newMember streams)
                                (allMixers st),
                            allDsps =
                              Map.insert
                                dsp
                                (capacity - 1)
                                (allDsps st)
                          }
                      )
              _ ->
                replyTo replyBox False >> return st
    Blocking (RemoveFromMixer theMixer theMember) replyBox -> do
      replyTo replyBox ()
      case Map.lookup theMixer (allMixers st) of
        Just (theDsp, theMembers)
          | Set.member theMember theMembers ->
            return
              st
                { allDsps =
                    Map.update
                      (Just . (+ 1))
                      theDsp
                      (allDsps st),
                  allMixers =
                    Map.insert
                      theMixer
                      (theDsp, Set.delete theMember theMembers)
                      (allMixers st)
                }
        _ ->
          return st
    NonBlocking (DestroyMixer theMixerId) ->
      let foundMixer (mixersDsp, mediaStreams) =
            st
              { allMixers =
                  Map.delete theMixerId (allMixers st),
                allDsps =
                  Map.update
                    (Just . (+ (1 + Set.size mediaStreams)))
                    mixersDsp
                    (allDsps st)
              }
       in pure $
            maybe
              st
              foundMixer
              (Map.lookup theMixerId (allMixers st))
    NonBlocking MediaShutdown ->
      pure st {shuttingDown = True}

data MediaSimSt = MediaSimSt
  { allDsps :: !(Map DspId Capacity),
    nextMixerId :: !MixerId,
    allMixers :: !(Map MixerId (DspId, Set MemberId)),
    shuttingDown :: !Bool
  }

mkMediaSimSt :: Map DspId Capacity -> MediaSimSt
mkMediaSimSt x =
  MediaSimSt
    { allDsps = x,
      nextMixerId = 0,
      allMixers = Map.empty,
      shuttingDown = False
    }