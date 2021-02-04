module BrokerTest (test) where

import Control.Exception (throw) 
import GHC.Stack
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.MessageBox.Broker
import UnliftIO.MessageBox.Class
import UnliftIO.MessageBox.Unlimited
import Utils

noBrokerConfig :: BrokerConfig k w' w f m a
noBrokerConfig =
  MkBrokerConfig
    { messageToBrokerAction = error "unexpected invokation: messageToBrokerAction",
      workerMessageBoxArg = error "unexpected invokation: workerMessageBoxArg",
      initWorker = error "unexpected invokation: initWorker",
      terminateWorker = error "unexpected invokation: terminateWorker",
      workerLoop = error "unexpected invokation: workerLoop"
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
            "when broker message box receive throws an exception,\
            \ then waiting on the async will return the exception"
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
            "when evaluation of the first incoming message causes an exception, \
            \ then the broker exits with that exception"
            $ do
              (Right (brokerIn, brokerA)) <-
                spawnBroker @_ @() @() @() @NoOpArg
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
                  noBrokerConfig
              deliver brokerIn Nothing
                >>= assertBool "deliver should succeed"
              r <- waitCatch brokerA
              assertEqual
                "exception expected"
                ( show
                    ( Left (SomeException expectedException) ::
                        Either SomeException ()
                    )
                )
                (show r),
          testCase
            "when a broker is cancelled while waiting for the first message,\
            \ while the broker is waiting for incoming messages,\
            \ then the broker exits with AsyncCancelled"
            $ do
              goOn <- newEmptyMVar
              (Right (_brokerIn, brokerA)) <-
                spawnBroker @_ @() @() @() @NoOpArg
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
            "when a broker receives 'Nothing' as the first message,\
            \ then the broker exits normally"
            $ do
              (Right (_brokerIn, brokerA)) <-
                spawnBroker @_ @() @() @() @NoOpArg
                  ( MkMsgBoxBuilder
                      ( return
                          ( MkMsgBox
                              ( return
                                  (OnDeliver (const (pure True)))
                              )
                              (return Nothing)
                              (error "unexpected invokation: tryReceive")
                          )
                      )
                      Nothing
                  )
                  noBrokerConfig
              r <- waitCatch brokerA
              assertEqual
                "success expected"
                "Right ()"
                (show r),
          testCase
            "when a broker receives a message for a missing worker,\
            \ it silently drops the message"
            $ do
              let brokerCfg =
                    MkBrokerConfig
                      { messageToBrokerAction = ((),) . Forward,
                        workerMessageBoxArg = NoOpArg,
                        initWorker = const (error "unexpected invokation: initWorker"),
                        terminateWorker = MkTerminateWorker (const (const (pure ()))) 0,
                        workerLoop = const (error "unexpected invokation: workerLoop")
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker @_ @() @_ @_ BlockingUnlimited brokerCfg
              deliver brokerIn (Just ("test-message" :: String))
                >>= assertBool "deliver should succeed"
              deliver brokerIn Nothing
                >>= assertBool "deliver should succeed"
              r <- waitCatch brokerA
              assertEqual
                "success expected"
                "Right ()"
                (show r),
          testCase
            "when a broker receives a STOP message for a missing worker,\
            \ it silently drops the message"
            $ do
              let brokerCfg =
                    MkBrokerConfig
                      { messageToBrokerAction = ((),) . ForwardAndStop . Just,
                        workerMessageBoxArg = NoOpArg,
                        initWorker = const (error "unexpected invokation: initWorker"),
                        terminateWorker = MkTerminateWorker (const (const (pure ()))) 0,
                        workerLoop = const (error "unexpected invokation: workerLoop")
                      }
              (Right (brokerIn, brokerA)) <-
                spawnBroker @_ @() @_ @_ BlockingUnlimited brokerCfg
              deliver brokerIn (Just ("test-message" :: String))
                >>= assertBool "deliver should succeed"
              deliver brokerIn Nothing
                >>= assertBool "deliver should succeed"
              r <- waitCatch brokerA
              assertEqual
                "success expected"
                "Right ()"
                (show r)
        ]
    ]