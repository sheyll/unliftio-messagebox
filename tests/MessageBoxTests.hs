module MessageBoxTests where

import Protocol.MessageBox
import qualified Test.Tasty as Tasty
import UnliftIO

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Protocols.MessageBox"
    []


{- 

manyReadersOneWriter = error "TODO"

manyWriterOneReader = error "TODO"

manyOneToOne = error "TODO"

-}