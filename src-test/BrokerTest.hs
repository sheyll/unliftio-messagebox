module BrokerTest (test) where

import GHC.Stack
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.MessageBox.Broker
import Utils

test :: HasCallStack => TestTree
test =
  testGroup
    "BrokerTests"
    [ testGroup
        "broker-startup"
        [ testCase "brokerMessageBox creation throws a SyncException" $ do
            let expectedException = stringException "Test"
            Just (Left a) <-
              timeout 1000000 $ 
              spawnBroker @_ @() @() @() @NoOpArg
                (MkMsgBoxBuilder @NoOpBox (throwIO expectedException) Nothing)
                ( MkBrokerConfig
                    { messageToBrokerAction = error "not required",
                      workerMessageBoxArg = error "not required",
                      initWorker = error "not required",
                      terminateWorker = error "not required",
                      workerLoop = error "not required"
                    }
                )
            assertEqual
              "exception expected"
              (show (SomeException expectedException))
              (show a)
        ]
    ]