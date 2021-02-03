module BrokerTest (test) where

import GHC.Stack
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.MessageBox.Broker
import Utils

noBrokerConfig :: BrokerConfig k w' w f m a
noBrokerConfig = MkBrokerConfig
                      { messageToBrokerAction = error "unexpected invokation",
                        workerMessageBoxArg = error "unexpected invokation",
                        initWorker = error "unexpected invokation",
                        terminateWorker = error "unexpected invokation",
                        workerLoop = error "unexpected invokation"
                      }

test :: HasCallStack => TestTree
test =
  testGroup
    "BrokerTests"
    [       
      testGroup
        "broker-startup"
        [ testCase "broker message box creation throws an exception" $ do
            let expectedException = stringException "Test"
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
          testCase "broker message box input creation throws an exception" $ do
            let expectedException = stringException "Test"
            Just (Left a) <-
              timeout 1000000 $
                spawnBroker @_ @() @() @() @NoOpArg
                  ( MkMsgBoxBuilder
                      ( return
                          ( MkMsgBox @NoOpInput
                              (throwIO expectedException)
                              (error "unexpected invokation")
                              (error "unexpected invokation")
                          )
                      )
                      Nothing
                  )
                  noBrokerConfig
            assertEqual
              "exception expected"
              (show (SomeException expectedException))
              (show a),
          testCase "broker message box receive throws an exception" $ do
            let expectedException = stringException "Test"
            Just (Right (_, a)) <-
              timeout 1000000 $
                spawnBroker @_ @() @() @() @NoOpArg
                  ( MkMsgBoxBuilder
                      ( return
                          ( MkMsgBox
                              ( return
                                  (OnDeliver (error "unexpected invokation"))
                              )
                              (throwIO expectedException)
                              (error "unexpected invokation")
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
              (show r)
        ]
    ]