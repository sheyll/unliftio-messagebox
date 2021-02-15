module PoolTest (test) where

import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO
import UnliftIO.MessageBox
import Utils

test :: TestTree
test =
  testGroup
    "Pool"
    [ testCase
        "when broker creation throws an exception, the process doesn't\
        \ block and returns an error"
        $ do
          let expectedException = stringException "expected exception"
              badPoolBox =
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
          Left e <-
            spawnPool @() @()
              badPoolBox
              BlockingUnlimited
              MkPoolWorkerCallback
                { runPoolWorkerCallback = const (const (return ()))
                }
          assertEqual "error expected" (show expectedException) (show e)
    ]