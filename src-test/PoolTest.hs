module PoolTest (test) where

import qualified Data.Atomics.Counter as Atomic
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.MessageBox
import Utils
import Data.Maybe (isNothing)

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
                "when delivering an Initialize with (Just Nothing) as payload,\
                \ the worker will be cleaned up and not be in the pool"
                $ do
                  counter <- Atomic.newCounter 0                                    
                  let cb = MkPoolWorkerCallback $ \() box -> do
                        Atomic.incrCounter_ 1 counter
                        receive box 
                          >>= maybe (return ()) 
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
                  cancel (poolAsync pool)

            ]
        ]
