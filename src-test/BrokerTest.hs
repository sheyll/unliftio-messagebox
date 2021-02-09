module BrokerTest (test) where

import Control.Exception (throw)
import Control.Monad (void, when)
import Data.List (sort)
import GHC.Stack
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.MessageBox.Broker
import UnliftIO.MessageBox.Class
import UnliftIO.MessageBox.Unlimited
import Utils
  ( MsgBox (MkMsgBox),
    MsgBoxBuilder (MkMsgBoxBuilder),
    NoOpArg (..),
    NoOpBox,
    NoOpInput (..),
  )

noBrokerConfig :: BrokerConfig k w' w a m
noBrokerConfig =
  MkBrokerConfig
    { demultiplexer = const $ error "unexpected invokation: demultiplexer",
      messageDispatcher = const $ error "unexpected invokation: messageDispatcher",
      resourceCreator = const $ error "unexpected invokation: resourceCreator",
      resourceCleaner = const $ error "unexpected invokation: resourceCleaner"
    }

expectedException :: StringException
expectedException = stringException "Test"

test :: HasCallStack => TestTree
test =
  testGroup
    "BrokerTests"
    [ testGroup
        "broker-startup"
        [ testCase
            "when a broker message box creation throws an exception, \
            \ the exception is returned in a Left..."
            $ do
              Just (Left a) <-
                timeout 1000000 $
                  spawnBroker @_ @() @() @() @NoOpArg
                    ( MkMsgBoxBuilder @NoOpBox
                        (throwIO expectedException)
                        Nothing
                    )
                    noBrokerConfig
              assertEqual
                "exception expected"
                (show (SomeException expectedException))
                (show a),
          testCase
            "when a broker message box input creation throws an exception,\
            \ the exception is returned in a Left..."
            $ do
              Just (Left a) <-
                timeout 1000000 $
                  spawnBroker @_ @() @() @() @NoOpArg
                    ( MkMsgBoxBuilder
                        ( return
                            ( MkMsgBox @NoOpInput
                                (throwIO expectedException)
                                (error "unexpected invokation: receive")
                                (error "unexpected invokation: tryReceive")
                            )
                        )
                        Nothing
                    )
                    noBrokerConfig
              assertEqual
                "exception expected"
                (show (SomeException expectedException))
                (show a),
          testCase
            "when the receive function throws a synchronous exception,\
            \ then waiting on the broker will return the exception"
            $ do
              Just (Right (_, a)) <-
                timeout 1000000 $
                  spawnBroker @_ @() @() @() @NoOpArg
                    ( MkMsgBoxBuilder
                        ( return
                            ( MkMsgBox
                                ( return
                                    (OnDeliver (error "unexpected invokation: OnDeliver"))
                                )
                                (throwIO expectedException)
                                (error "unexpected invokation: tryReceive")
                            )
                        )
                        Nothing
                    )
                    noBrokerConfig
              r <- waitCatch a
              assertEqual
                "exception expected"
                ( show
                    ( Left (SomeException expectedException) ::
                        Either SomeException ()
                    )
                )
                (show r),
          testCase
            "when evaluation of an incoming message causes an exception,\
            \ then the broker ignores the error and continues"
            $ do
              (Right (brokerIn, brokerA)) <-
                spawnBroker
                  ( MkMsgBoxBuilder
                      ( return
                          ( MkMsgBox
                              ( return
                                  (OnDeliver (const (pure True)))
                              )
                              (return (Just (throw expectedException)))
                              (error "unexpected invokation: tryReceive")
                          )
                      )
                      Nothing
                  )
                  ( MkBrokerConfig
                      { demultiplexer = Dispatch (),
                        messageDispatcher =
                          const (error "unexpected invokation: messageDispatcher"),
                        resourceCreator =
                          const (error "unexpected invokation: resourceCreator"),
                        resourceCleaner =
                          const (error "unexpected invokation: resourceCleaner")
                      }
                  )
              deliver_ brokerIn ()
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "Left AsyncCancelled"
                (show r),
          testCase
            "when evaluation of the first incoming message causes an async\
            \ exception, then the broker exits with that exception"
            $ do
              (Right (brokerIn, brokerA)) <-
                spawnBroker
                  ( MkMsgBoxBuilder
                      ( return
                          ( MkMsgBox
                              ( return
                                  (OnDeliver (const (pure True)))
                              )
                              (return (Just (throw (AsyncExceptionWrapper expectedException))))
                              (error "unexpected invokation: tryReceive")
                          )
                      )
                      Nothing
                  )
                  ( MkBrokerConfig
                      { demultiplexer = Dispatch (),
                        messageDispatcher =
                          const (error "unexpected invokation: messageDispatcher"),
                        resourceCreator =
                          const (error "unexpected invokation: resourceCreator"),
                        resourceCleaner =
                          const (error "unexpected invokation: resourceCleaner")
                      }
                  )
              deliver_ brokerIn ()
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                ( show
                    ( Left (SomeException (AsyncExceptionWrapper expectedException)) ::
                        Either SomeException ()
                    )
                )
                (show r),
          testCase
            "when a broker is cancelled while waiting for the first message,\
            \ then the broker exits with AsyncCancelled"
            $ do
              goOn <- newEmptyMVar
              (Right (_brokerIn, brokerA)) <-
                spawnBroker @_ @() @() @()
                  ( MkMsgBoxBuilder
                      ( return
                          ( MkMsgBox
                              ( return
                                  (OnDeliver (const (pure True)))
                              )
                              ( do
                                  putMVar goOn ()
                                  threadDelay 1_000_000
                                  return (Just (error "unexpected evaluation"))
                              )
                              (error "unexpected invokation: tryReceive")
                          )
                      )
                      Nothing
                  )
                  noBrokerConfig
              takeMVar goOn
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "Left AsyncCancelled"
                (show r),
          testCase
            "when a broker receives a message for a missing resource,\
            \ it silently drops the message"
            $ do
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = Dispatch (),
                        messageDispatcher =
                          const (error "unexpected invokation: messageDispatcher"),
                        resourceCreator =
                          const (error "unexpected invokation: resourceCreator"),
                        resourceCleaner =
                          const (error "unexpected invokation: resourceCleaner")
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn ()
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "success expected"
                "Left AsyncCancelled"
                (show r),
          testCase
            "when an empty broker receives a start message without payload\
            \ and the creator callback throws an exception,\
            \ a normal message for that key will be ignored,\
            \ no cleanup is performed, and the broker lives on"
            $ do
              workerInitialized <- newEmptyMVar
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer =
                          \m ->
                            if m
                              then Initialize () Nothing
                              else Dispatch () (),
                        messageDispatcher =
                          const (error "unexpected invokation: messageDispatcher"),
                        resourceCreator = \_k _mw -> do
                          putMVar workerInitialized ()
                          throwIO expectedException,
                        resourceCleaner =
                          const (error "unexpected invokation: resourceCleaner")
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn True
              timeout 1000000 (takeMVar workerInitialized)
                >>= assertEqual
                  "resourceCreator wasn't executed"
                  (Just ())
              deliver_ brokerIn False
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "Left AsyncCancelled"
                (show r),
          testCase
            "when an empty broker receives a start message with a payload\
            \ and when the MessageHandler callback throws an exception when\
            \ applied to that payload, cleanup is performed once, and\
            \ incoming messages for that key will be ignored,\
            \ and the broker lives on"
            $ do
              cleanupCalls <- newEmptyMVar
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer =
                          \isInitPayload ->
                            if isInitPayload
                              then Initialize () (Just True)
                              else Dispatch () False,
                        messageDispatcher =
                          \() isInitPayload _ ->
                            if isInitPayload
                              then throwIO expectedException
                              else error "unexpected invokation: messageDispatcher",
                        resourceCreator = \_k _mw -> do
                          putMVar cleanupCalls (0 :: Int)
                          return (),
                        resourceCleaner =
                          \() () ->
                            modifyMVar
                              cleanupCalls
                              (\cnt -> return (cnt + 1, ()))
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn True
              deliver_ brokerIn False
              threadDelay 10_000
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "Left AsyncCancelled"
                (show r)
              takeMVar cleanupCalls
                >>= assertEqual "resourceCleaner wasn't executed" 1
              -- prevent GC of msg box input:
              deliver_ brokerIn False,
          testCase
            "when 3 resources are initialized and then the broker is cancelled,\
            \ cleanup is performed foreach resource."
            $ do
              resourceCreated <- newEmptyMVar
              cleanupCalled <- newTVarIO []

              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \k -> Initialize k (Just k),
                        messageDispatcher = \k _w _a -> do
                          putMVar resourceCreated k
                          threadDelay 10_000 -- delay here so we make sure to
                          -- be cancelled before we leave this
                          -- function.
                          -- Now if the implementation isn't
                          -- handling async exceptions well,
                          -- the resource isn't in the resource
                          -- map when cleanup is called,
                          -- and hence won't be properly
                          -- cleaned up!
                          return KeepResource,
                        resourceCreator =
                          \k _mw -> return k,
                        resourceCleaner =
                          \k a ->
                            atomically (modifyTVar cleanupCalled ((k, a) :))
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              deliver_ brokerIn 2
              deliver_ brokerIn 3
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 1
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 2
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 3
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "Left AsyncCancelled"
                (show r)
              readTVarIO cleanupCalled
                >>= assertEqual "resourceCleaner wasn't executed" [(1, 1), (2, 2), (3, 3)]
                  . sort
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when 3 resources are initialized and then the broker is cancelled,\
            \ cleanup is performed foreach resource, even if exceptions are thrown\
            \ in the cleanup callbacks"
            $ do
              resourceCreated <- newEmptyMVar
              cleanupCalled <- newTVarIO []
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \k -> Initialize k (Just k),
                        messageDispatcher = \k _w _a -> do
                          putMVar resourceCreated k
                          threadDelay 10_000 -- delay here so we make sure to
                          -- be cancelled before we leave this
                          -- function.
                          -- Now if the implementation isn't
                          -- handling async exceptions well,
                          -- the resource isn't in the resource
                          -- map when cleanup is called,
                          -- and hence won't be properly
                          -- cleaned up!
                          return KeepResource,
                        resourceCreator =
                          \k _mw -> return k,
                        resourceCleaner =
                          \k a -> do
                            atomically (modifyTVar cleanupCalled ((k, a) :))
                            throwIO expectedException
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              deliver_ brokerIn 2
              deliver_ brokerIn 3
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 1
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 2
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 3
              cancel brokerA
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "Left AsyncCancelled"
                (show r)
              readTVarIO cleanupCalled
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  [(1, 1), (2, 2), (3, 3)]
                  . sort
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when 2 resources are added and\
            \ while adding a 3rd an async exception is thrown\
            \ when handling the initial message,\
            \ then the broker cleans up the 3 resources and exists"
            $ do
              resourceCreated <- newEmptyMVar
              cleanupCalled <- newTVarIO []
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \k -> Initialize k (Just k),
                        messageDispatcher = \k _w _a -> do
                          putMVar resourceCreated k
                          return KeepResource,
                        resourceCreator =
                          \k _mw -> return k,
                        resourceCleaner =
                          \k a -> do
                            atomically (modifyTVar cleanupCalled ((k, a) :))
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              deliver_ brokerIn 2
              deliver_ brokerIn 3
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 1
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 2
              takeMVar resourceCreated >>= assertEqual "invalid resource created" 3
              throwTo (asyncThreadId brokerA) expectedException
              Left r <- waitCatch brokerA
              readTVarIO cleanupCalled
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  [(1, 1), (2, 2), (3, 3)]
                  . sort
              assertEqual
                "exception expected"
                (show expectedException)
                (show r)
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when adding a new resource with an extra initial message,\
            \ when the messageHandler returns KeepResource,\
            \ then the resource returned from the create callback is passed to\
            \ the cleanup function"
            $ do
              resourceCreated <- newEmptyMVar
              cleanupCalled <- newEmptyTMVarIO
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \_k -> Initialize () (Just ()),
                        messageDispatcher = \_k _w a -> do
                          putMVar resourceCreated a
                          return KeepResource,
                        resourceCreator =
                          \_k _mw -> return initialResource,
                        resourceCleaner =
                          \_k a -> do
                            atomically (putTMVar cleanupCalled a)
                      }
                  initialResource :: Int
                  initialResource = 123
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              takeMVar resourceCreated
                >>= assertEqual "invalid resource created" initialResource
              cancel brokerA
              Left r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "AsyncCancelled"
                (show r)
              atomically (takeTMVar cleanupCalled)
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  initialResource
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when adding a new resource with an extra initial message,\
            \ when the messageHandler returns (UpdateResource x),\
            \ then x is passed to the cleanup function"
            $ do
              resourceCreated <- newEmptyMVar
              cleanupCalled <- newEmptyTMVarIO
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \_k -> Initialize () (Just ()),
                        messageDispatcher = \_k _w a -> do
                          void $ async (threadDelay 100000 >> putMVar resourceCreated a)
                          return (UpdateResource x),
                        resourceCreator =
                          \_k _mw -> return initialResource,
                        resourceCleaner =
                          \_k -> atomically . putTMVar cleanupCalled
                      }
                  initialResource :: Int
                  initialResource = 123
                  x :: Int
                  x = 234
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              takeMVar resourceCreated
                >>= assertEqual "invalid resource created" initialResource
              cancel brokerA
              Left r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "AsyncCancelled"
                (show r)
              atomically (takeTMVar cleanupCalled)
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  x
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when adding a new resource with an extra initial message,\
            \ when the messageHandler returns (RemoveResource Nothing),\
            \ then the initial resource is passed to the cleanup function"
            $ do
              resourceCreated <- newEmptyMVar
              readyForCleanup <- newEmptyMVar
              cleanupCalled <- newEmptyTMVarIO
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \_k -> Initialize () (Just ()),
                        messageDispatcher = \_k _w a -> do
                          void . async $ do
                            () <- takeMVar readyForCleanup
                            putMVar resourceCreated a
                          return (RemoveResource Nothing),
                        resourceCreator =
                          \_k _mw -> return initialResource,
                        resourceCleaner =
                          \_k a -> do
                            putMVar readyForCleanup ()
                            atomically (putTMVar cleanupCalled a)
                      }
                  initialResource :: Int
                  initialResource = 123
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              takeMVar resourceCreated
                >>= assertEqual "invalid resource created" initialResource
              cancel brokerA
              Left r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "AsyncCancelled"
                (show r)
              atomically (takeTMVar cleanupCalled)
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  initialResource
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when adding a new resource with an extra initial message,\
            \ when the messageHandler returns (RemoveResource (Just x)),\
            \ then x is passed to the cleanup function"
            $ do
              resourceCreated <- newEmptyMVar
              readyForCleanup <- newEmptyMVar
              cleanupCalled <- newEmptyTMVarIO
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \_k -> Initialize () (Just ()),
                        messageDispatcher = \_k _w a -> do
                          void . async $ do
                            () <- takeMVar readyForCleanup
                            putMVar resourceCreated a
                          return (RemoveResource (Just x)),
                        resourceCreator =
                          \_k _mw -> return initialResource,
                        resourceCleaner =
                          \_k a -> do
                            putMVar readyForCleanup ()
                            atomically (putTMVar cleanupCalled a)
                      }
                  initialResource :: Int
                  initialResource = 123
                  x :: Int
                  x = 234
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              takeMVar resourceCreated
                >>= assertEqual "invalid resource created" initialResource
              cancel brokerA
              Left r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "AsyncCancelled"
                (show r)
              atomically (takeTMVar cleanupCalled)
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  x
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "when adding a new resource without an extra initial message,\
            \ then the initial resource is passed to the cleanup function"
            $ do
              resourceCreated <- newEmptyMVar
              cleanupCalled <- newEmptyTMVarIO
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \_k -> Initialize () (Just ()),
                        messageDispatcher = \_k _w a -> do
                          void $ async (threadDelay 100000 >> putMVar resourceCreated a)
                          return (UpdateResource x),
                        resourceCreator =
                          \_k _mw -> return initialResource,
                        resourceCleaner =
                          \_k -> atomically . putTMVar cleanupCalled
                      }
                  initialResource :: Int
                  initialResource = 123
                  x :: Int
                  x = 234
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int)
              takeMVar resourceCreated
                >>= assertEqual "invalid resource created" initialResource
              cancel brokerA
              Left r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "AsyncCancelled"
                (show r)
              atomically (takeTMVar cleanupCalled)
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  x
              -- prevent GC of msg box input:
              deliver_ brokerIn 666,
          testCase
            "on a broker with two resources a and b with keys 1 and 2,\
            \ for an incoming message with key=1 the MessageDispatcher is\
            \ called with resource a and for key=2 with resource b"
            $ do
              messageDispatched <- newEmptyMVar
              cleanupCalled <- newTVarIO []
              let brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \(k, isInit) ->
                          if isInit
                            then Initialize k Nothing
                            else Dispatch k k,
                        resourceCreator =
                          \k _mw -> return k,
                        messageDispatcher = \k _w a -> do
                          putMVar messageDispatched (k, a)
                          return KeepResource,
                        resourceCleaner =
                          \k a -> do
                            atomically (modifyTVar cleanupCalled ((k, a) :))
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn (1 :: Int, True)
              deliver_ brokerIn (2, True)
              deliver_ brokerIn (1, False)
              deliver_ brokerIn (2, False)
              takeMVar messageDispatched
                >>= assertEqual "invalid resource" (1, 1)
              takeMVar messageDispatched
                >>= assertEqual "invalid resource" (2, 2)
              throwTo (asyncThreadId brokerA) expectedException
              Left r <- waitCatch brokerA
              readTVarIO cleanupCalled
                >>= assertEqual
                  "resourceCleaner wasn't executed"
                  [(1, 1), (2, 2)]
                  . sort
              assertEqual
                "exception expected"
                (show expectedException)
                (show r)
              -- prevent GC of msg box input:
              deliver_ brokerIn (666, False),
          testCase
            "When the MessageDispatcher is called with resource x and \
            \ returns (UpdateResource y), then next time it is called with\
            \ resource y"
            $ do
              messageDispatched <- newEmptyMVar
              let x :: Int
                  x = 42
                  y = x + 1295
                  brokerCfg =
                    MkBrokerConfig
                      { demultiplexer = \isInit ->
                          if isInit
                            then Initialize () Nothing
                            else Dispatch () (),
                        resourceCreator = \_k _mw ->
                          return x,
                        messageDispatcher = \_k _w a -> do
                          putMVar messageDispatched a
                          return (UpdateResource y),
                        resourceCleaner = \_k _a ->
                          return ()
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker BlockingUnlimited brokerCfg
              deliver_ brokerIn True
              deliver_ brokerIn False
              deliver_ brokerIn False
              takeMVar messageDispatched
                >>= assertEqual "invalid resource" x
              takeMVar messageDispatched
                >>= assertEqual "invalid resource" y
              cancel brokerA
              Left r <- waitCatch brokerA
              assertEqual
                "exception expected"
                "AsyncCancelled"
                (show r)
              -- prevent GC of msg box input:
              deliver_ brokerIn False,
          testCase
            "when the receive function throws a synchronous exception x,\
            \ then all resources will be cleaned up and\
            \ the broker will re-throw x"
            $ do
              cr <- newTVarIO [] :: IO (TVar [Int])
              cl <- newTVarIO []
              done <- newEmptyMVar
              let bCfg =
                    MkBrokerConfig
                      { demultiplexer = (`Initialize` Nothing),
                        resourceCreator = \k _mw -> do
                          atomically $
                            modifyTVar cr (k :)
                          return (),
                        messageDispatcher = \_k _w _a ->
                          return KeepResource,
                        resourceCleaner = \k _a -> do
                          rs <- atomically $ do
                            modifyTVar cl (k :)
                            readTVar cl
                          when (rs == [3,2,1]) $
                            putMVar done ()
                      }
                  inter =
                    MkInterceptingBoxArg
                      ( MkInterceptor $ do
                          rs <- readTVarIO cr
                          if rs == [3,2,1]
                            then throwIO expectedException
                            else return Nothing
                      )
                      (MkInterceptor (return Nothing))
                      BlockingUnlimited
              Right (b, bA) <- spawnBroker inter bCfg
              deliver_ b (1 :: Int)
              deliver_ b (2 :: Int)
              deliver_ b (3 :: Int)
              deliver_ b (4 :: Int)
              takeMVar done  
              Left ex <- waitCatch bA
              assertEqual
                "exception expected"
                (show expectedException)
                (show ex)
              -- prevent GC of msg box input:
              deliver_ b 666
        ]
    ]

newtype Interceptor = MkInterceptor
  { runInterceptor ::
      forall m.
      MonadUnliftIO m =>
      m (Maybe Bool)
  }

data InterceptingBoxArg b
  = MkInterceptingBoxArg
      Interceptor -- receive interceptor
      Interceptor -- deliver interceptor
      b

data InterceptingBox b a
  = MkInterceptingBox
      Interceptor -- receive interceptor
      Interceptor -- deliver interceptor
      (b a)

data InterceptingBoxIn i a
  = MkInterceptingBoxIn
      Interceptor
      (i a)

instance IsMessageBoxArg b => IsMessageBoxArg (InterceptingBoxArg b) where
  type MessageBox (InterceptingBoxArg b) = InterceptingBox (MessageBox b)
  newMessageBox (MkInterceptingBoxArg r i a) =
    MkInterceptingBox r i <$> newMessageBox a
  getConfiguredMessageLimit (MkInterceptingBoxArg _ _ a) =
    getConfiguredMessageLimit a

instance IsMessageBox b => IsMessageBox (InterceptingBox b) where
  type Input (InterceptingBox b) = InterceptingBoxIn (Input b)
  newInput (MkInterceptingBox _ i b) =
    MkInterceptingBoxIn i <$> newInput b
  receive (MkInterceptingBox r _ b) = do
    mi <- runInterceptor r
    case mi of
      Nothing ->
        receive b
      Just True ->
        receive b
      Just False ->
        return Nothing
  tryReceive (MkInterceptingBox r _ b) = do
    -- oh my ... dunno what to do in this case...
    _mi <- runInterceptor r
    tryReceive b

instance IsInput i => IsInput (InterceptingBoxIn i) where
  deliver (MkInterceptingBoxIn interceptIn i) x = do
    mi <- runInterceptor interceptIn
    case mi of
      Nothing ->
        deliver i x
      Just o ->
        return o