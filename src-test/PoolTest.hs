module PoolTest (test) where

import Control.Monad (replicateM_)
import qualified Data.Atomics.Counter as Atomic
import Data.Function (fix)
import Data.Maybe (isNothing)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.MessageBox
import Utils

test :: TestTree
test =
  let expectedException = stringException "expected exception"
   in testGroup
        "Pool"
        [ testGroup
            "empty pool"
            [ testCase
                "when broker creation throws an exception, the process doesn't\
                \ block and returns an error"
                $ do
                  let badPoolBox =
                        MkMockBoxInit
                          ( return
                              ( MkMockBox
                                  (throwIO expectedException)
                                  (error "unexpected invokation: receive")
                                  (error "unexpected invokation: tryReceive")
                              )
                          )
                          Nothing
                  Left e <-
                    spawnPool @() @() @(MockBoxInit (MockBox NoOpInput))
                      badPoolBox
                      BlockingUnlimited
                      MkPoolWorkerCallback
                        { runPoolWorkerCallback = const (const (return ()))
                        }
                  assertEqual "error expected" (show expectedException) (show e),
              testCase
                "when receiving from the pool input throws an exception,\
                \ the pool exits with that exception"
                $ do
                  let badPoolBox =
                        MkMockBoxInit
                          ( return
                              ( MkMockBox
                                  ( return
                                      (OnDeliver (error "unexpected invokation: OnDeliver"))
                                  )
                                  (throwIO expectedException)
                                  (error "unexpected invokation: tryReceive")
                              )
                          )
                          Nothing
                  Right pool <-
                    spawnPool @() @()
                      badPoolBox
                      BlockingUnlimited
                      MkPoolWorkerCallback
                        { runPoolWorkerCallback = const (const (return ()))
                        }
                  Left e <- waitCatch (poolAsync pool)
                  assertEqual "error expected" (show expectedException) (show e),
              testCase
                "when receiving from the pool input throws an exception,\
                \ the pool exits with that exception"
                $ do
                  let badPoolBox =
                        MkMockBoxInit
                          ( return
                              ( MkMockBox
                                  ( return
                                      (OnDeliver (error "unexpected invokation: OnDeliver"))
                                  )
                                  (throwIO expectedException)
                                  (error "unexpected invokation: tryReceive")
                              )
                          )
                          Nothing
                  Right pool <-
                    spawnPool @() @()
                      badPoolBox
                      BlockingUnlimited
                      MkPoolWorkerCallback
                        { runPoolWorkerCallback = const (const (return ()))
                        }
                  Left e <- waitCatch (poolAsync pool)
                  assertEqual "error expected" (show expectedException) (show e)
            ],
          testGroup
            "non-empty pool"
            [ testCase
                "when delivering an Initialize,\
                \ the worker callback is executed in a new process"
                $ do
                  workerIdRef <- newEmptyMVar
                  let cb = MkPoolWorkerCallback $ \() _box ->
                        myThreadId >>= putMVar workerIdRef
                  Right pool <- spawnPool @() BlockingUnlimited BlockingUnlimited cb
                  deliver_ (poolInput pool) (Initialize () (Just (Just ())))
                  workerId <- takeMVar workerIdRef
                  cancel (poolAsync pool)
                  assertBool
                    "worker and broker threadIds must be different"
                    (asyncThreadId (poolAsync pool) /= workerId),
              testCase
                "when delivering an Initialize and the worker message box creation fails,\
                \ the worker will be cleaned up and not be in the pool"
                $ do
                  workerIdRef <- newEmptyMVar
                  let badWorkerBox =
                        MkMockBoxInit
                          ( do
                              myThreadId >>= putMVar workerIdRef
                              throwIO expectedException
                          )
                          Nothing
                  let cb = MkPoolWorkerCallback $ \() _box ->
                        error "unexpected invokation: callback"
                  Right pool <-
                    spawnPool @() @_ @_ @(MockBoxInit (MockBox NoOpInput))
                      BlockingUnlimited
                      badWorkerBox
                      cb
                  deliver_ (poolInput pool) (Initialize () (Just (Just ())))
                  workerId1 <- takeMVar workerIdRef
                  deliver_ (poolInput pool) (Initialize () (Just (Just ())))
                  workerId2 <- takeMVar workerIdRef
                  cancel (poolAsync pool)
                  assertBool
                    "the broken worker must be removed from the pool"
                    (workerId1 /= workerId2),
              testCase
                "when delivering an Initialize and the worker message box input creation fails,\
                \ the worker will be cleaned up and not be in the pool"
                $ do
                  workerIdRef <- newEmptyMVar
                  let badWorkerBox =
                        MkMockBoxInit
                          ( return
                              ( MkMockBox
                                  ( do
                                      myThreadId >>= putMVar workerIdRef
                                      throwIO expectedException
                                  )
                                  (error "unexpected invokation: receive")
                                  (error "unexpected invokation: tryReceive")
                              )
                          )
                          Nothing
                  let cb = MkPoolWorkerCallback $ \() _box ->
                        error "unexpected invokation: callback"
                  Right pool <-
                    spawnPool @() @_ @_ @(MockBoxInit (MockBox NoOpInput))
                      BlockingUnlimited
                      badWorkerBox
                      cb
                  deliver_ (poolInput pool) (Initialize () (Just (Just ())))
                  workerId1 <- takeMVar workerIdRef
                  deliver_ (poolInput pool) (Initialize () (Just (Just ())))
                  workerId2 <- takeMVar workerIdRef
                  cancel (poolAsync pool)
                  assertBool
                    "the broken worker must be removed from the pool"
                    (workerId1 /= workerId2),
              testCase
                "when delivering an 'Initialize (Just Nothing)',\
                \ the worker will be cleaned up and not be in the pool"
                $ do
                  counter <- Atomic.newCounter 0
                  let cb = MkPoolWorkerCallback $ \() box -> do
                        Atomic.incrCounter_ 1 counter
                        receive box
                          >>= maybe
                            (return ())
                            (either id (`putMVar` ()))
                  Right pool <-
                    spawnPool
                      BlockingUnlimited
                      BlockingUnlimited
                      cb
                  deliver_ (poolInput pool) (Initialize () (Just Nothing))
                  threadDelay 1000
                  -- must not happen:
                  failed <- newEmptyMVar
                  deliver_ (poolInput pool) (Dispatch () (Just (Left (putMVar failed ()))))
                  timeout 10000 (takeMVar failed)
                    >>= assertBool "the worker should not be running" . isNothing
                  deliver_ (poolInput pool) (Initialize () Nothing)
                  done <- newEmptyMVar
                  deliver_ (poolInput pool) (Dispatch () (Just (Right done)))
                  takeMVar done
                  Atomic.readCounter counter
                    >>= assertEqual "invalid number of pool worker callback invokations" 2
                  cancel (poolAsync pool),
              testCase
                "when several workers are initialized with different keys,\
                \ all are created and available in the pool."
                $ do
                  startedRef <- newEmptyMVar
                  let cb = MkPoolWorkerCallback $ \k box -> do
                        putMVar startedRef k
                        fix $ \again -> do
                          m <- receive box
                          case m of
                            Nothing -> return ()
                            Just ref -> do
                              putMVar ref k
                              again

                  Right pool <-
                    spawnPool @Int
                      BlockingUnlimited
                      BlockingUnlimited
                      cb

                  deliver_ (poolInput pool) (Initialize 0 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 0
                  deliver_ (poolInput pool) (Initialize 1 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 1
                  deliver_ (poolInput pool) (Initialize 2 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 2
                  deliver_ (poolInput pool) (Initialize 3 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 3

                  workRef <- newEmptyMVar
                  deliver_ (poolInput pool) (Dispatch 0 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 0
                  deliver_ (poolInput pool) (Dispatch 1 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 1
                  deliver_ (poolInput pool) (Dispatch 2 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 2
                  deliver_ (poolInput pool) (Dispatch 3 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 3

                  cancel (poolAsync pool),
              testCase
                "when 'Dispatch k (Just x)' is delivered, and a worker k exists,\
                \ the worker will receive the message x, otherwise it will silently be dropped."
                $ do
                  startedRef <- newEmptyMVar
                  let cb = MkPoolWorkerCallback $ \k box -> do
                        putMVar startedRef k
                        fix $ \again -> do
                          m <- receive box
                          case m of
                            Nothing -> return ()
                            Just ref -> do
                              putMVar ref k
                              again

                  Right pool <-
                    spawnPool @Int
                      BlockingUnlimited
                      BlockingUnlimited
                      cb

                  deliver_ (poolInput pool) (Initialize 0 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 0
                  deliver_ (poolInput pool) (Initialize 1 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 1

                  workRef <- newEmptyMVar
                  deliver_ (poolInput pool) (Dispatch 0 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 0

                  deliver_ (poolInput pool) (Dispatch 666 (Just workRef))
                  timeout 10000 (takeMVar workRef)
                    >>= assertEqual "wrong workRef value" Nothing

                  cancel (poolAsync pool),
              testCase
                "when Dispatch k Nothing is delivered, and a worker k exists,\
                \ the worker will be cleaned up and removed from the pool"
                $ do
                  startedRef <- newEmptyMVar
                  let cb = MkPoolWorkerCallback $ \k box -> do
                        putMVar startedRef k
                        fix $ \again -> do
                          m <- receive box
                          case m of
                            Nothing -> return ()
                            Just ref -> do
                              putMVar ref k
                              again

                  Right pool <-
                    spawnPool @Int
                      BlockingUnlimited
                      BlockingUnlimited
                      cb

                  deliver_ (poolInput pool) (Initialize 0 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 0
                  deliver_ (poolInput pool) (Initialize 1 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 1

                  workRef <- newEmptyMVar
                  deliver_ (poolInput pool) (Dispatch 0 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 0

                  deliver_ (poolInput pool) (Dispatch 1 Nothing)

                  deliver_ (poolInput pool) (Dispatch 1 (Just workRef))
                  timeout 10000 (takeMVar workRef)
                    >>= assertEqual "wrong workRef value" Nothing

                  deliver_ (poolInput pool) (Dispatch 0 (Just workRef))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 0

                  cancel (poolAsync pool),
              testCase
                "when 'deliver' to a worker message box returns False,\
                \ the worker is removed (and cancelled)"
                $ do
                  startedRef <- newEmptyMVar
                  let cb = MkPoolWorkerCallback $ \k box -> do
                        putMVar startedRef k
                        fix $ \again -> do
                          m <- receive box
                          case m of
                            Nothing -> return ()
                            Just (Left ref) -> do
                              putMVar ref k
                              again
                            Just (Right ()) -> do
                              threadDelay 100000000

                  Right pool <-
                    spawnPool @Int
                      BlockingUnlimited
                      (NonBlockingBoxLimit MessageLimit_2)
                      cb

                  deliver_ (poolInput pool) (Initialize 0 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 0

                  deliver_ (poolInput pool) (Initialize 1 Nothing)
                  takeMVar startedRef >>= assertEqual "wrong startedRef value" 1

                  workRef <- newEmptyMVar

                  deliver_ (poolInput pool) (Dispatch 0 (Just (Left workRef)))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 0

                  -- Now overflow the input by first sending a Right message and then
                  -- so many messages, that the message limit will cause 'deliver'
                  -- to return 'False':
                  replicateM_
                    16
                    (deliver_ (poolInput pool) (Dispatch 0 (Just (Right ()))))

                  -- Now the process should be dead:
                  deliver_ (poolInput pool) (Dispatch 0 (Just (Left workRef)))
                  timeout 10000 (takeMVar workRef)
                    >>= assertEqual "wrong workRef value" Nothing

                  -- ... while the other process should be alive:
                  deliver_ (poolInput pool) (Dispatch 1 (Just (Left workRef)))
                  takeMVar workRef >>= assertEqual "wrong workRef value" 1

                  cancel (poolAsync pool)
            ]
        ]
