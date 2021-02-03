module CatchAllTest (test) where

import Data.Proxy
import UnliftIO.MessageBox.Util.Future
import UnliftIO.MessageBox.CatchAll
import UnliftIO.MessageBox.Class
import QCOrphans ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import UnliftIO
import Utils

test :: TestTree
test =
  testGroup
    "CatchAll"
    [ testProperty
        "Derived Classes Coverage"
        (allEqOrdShowMethodsImplemented (Proxy @(CatchAllArg Int))),
      testCase
        "when the wrapped receive throws an exception,\
        \ CatchAlls receive returns Nothing"
        $ receive (CatchAllBox BadBox)
          >>= assertEqual "receive should return Nothing" (Nothing :: Maybe ()),
      testCase
        "when the wrapped tryReceive throws an exception,\
        \ CatchAlls tryReceive returns a Future that will always return Nothing"
        $ do
          f <- tryReceive (CatchAllBox BadBox)
          r <- tryNow f
          assertEqual "The Future should return Nothing" (Nothing :: Maybe ()) r,
      testCase
        "when the wrapped receiveAfter throws an exception,\
        \ CatchAlls receiveAfter should return Nothing"
        $ receiveAfter (CatchAllBox BadBox) 123
          >>= assertEqual
            "receiveAfter should return Nothing"
            (Nothing :: Maybe Int),
      testCase
        "when the wrapped deliver throws an exception, CatchAlls deliver returns False"
        $ deliver (CatchAllInput BadInput) ()
          >>= assertEqual "deliver should return False" False
    ]

data BadBox a = BadBox

data BadInput a = BadInput

instance IsMessageBox BadBox where
  type Input BadBox = BadInput
  receive _ = throwIO (stringException "test")
  tryReceive _ = throwIO (stringException "test")
  receiveAfter _ _ = throwIO (stringException "test")
  newInput _ = return BadInput

instance IsInput BadInput where
  deliver _ _ = throwIO (stringException "test")
