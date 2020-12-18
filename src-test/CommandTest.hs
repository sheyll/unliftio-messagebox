{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module CommandTest where

import Data.Functor
import Data.Semigroup
import Protocol.Command
import Data.Maybe(isJust)

import Protocol.MessageBoxClass
  ( IsMessageBox (newInBox, newOutBox),
  )
import Protocol.UnboundedMessageBox (InBoxConfig (UnboundedMessageBox))
import Test.Tasty as Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Large (getLarge),
    NonEmptyList (getNonEmpty),
    Positive (..),
    PrintableString (..),
    Property,
    ioProperty,
    testProperty,
  )
import UnliftIO (conc, runConc)
import UnliftIO.Concurrent
import Test.Tasty.HUnit (assertEqual
      ,assertBool, assertFailure, testCase)

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.Command"
    [ testProperty
        "all books that many donors concurrently donate into the book store end up in the bookstore"
        allDonatedBooksAreInTheBookStore,
      testCase "handling a cast succeeds" $ do
        bookStoreInBox <- newInBox UnboundedMessageBox
        bookStoreOutBox <- newOutBox bookStoreInBox
        let 
          expected = Donate
                  (Donor (PersonName "Mueller" "Hans") 1)
                  ( Book
                      "Wann wenn nicht wir."
                      (Author (PersonName "Schmitt" "Agent") 477)
                      (Publisher "Kletten Verlag")
                      (Year 2019)
                      []
                  )
              
        void (forkIO ( void (cast bookStoreOutBox expected) ) )
        result <- handleMessage bookStoreInBox $
            \case 
              NonBlocking actual ->
                assertEqual "correct message" expected actual
              a ->
                assertFailure ("unexpected message: " <> show a)
        assertBool "handling the message was successful" (isJust result)
    ]

allDonatedBooksAreInTheBookStore :: [(Donor, Book)] -> Property
allDonatedBooksAreInTheBookStore donorsAndBooks = ioProperty $ do
  bookStoreIn <- newInBox UnboundedMessageBox
  bookStoreOut <- newOutBox bookStoreIn
  getAll
    <$> runConc
      ( foldMap
          ( \(donor, book) ->
              conc (cast bookStoreOut (Donate donor book) $> All True)
          )
          donorsAndBooks
          <> pure (All True)
      )

data BookStore

data instance Command BookStore _ where
  Donate :: Donor -> Book -> Command BookStore 'FireAndForget
  GetBooks :: Command BookStore ('Return [Book])

deriving stock instance Eq (Command BookStore 'FireAndForget)
deriving stock instance Show (Command BookStore 'FireAndForget)

deriving stock instance Eq (Command BookStore ('Return [Book]))
deriving stock instance Show (Command BookStore ('Return [Book]))

newtype Year = Year Int
  deriving newtype (Show, Eq, Ord)
  deriving (Arbitrary) via (Positive Int)

newtype Page = Page String
  deriving newtype (Show, Eq, Ord)
  deriving (Arbitrary) via PrintableString

data PersonName = PersonName
  { lastName :: String,
    surName :: String
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary PersonName where
  arbitrary =
    PersonName
      <$> (getPrintableString <$> arbitrary)
        <*> (getPrintableString <$> arbitrary)

data Donor = Donor
  { donorName :: PersonName,
    donorId :: Int
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Donor where
  arbitrary =
    Donor
      <$> arbitrary
      <*> (getLarge . getPositive <$> arbitrary)

data Author = Author
  { authorName :: PersonName,
    authorId :: Int
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Author where
  arbitrary =
    Author
      <$> arbitrary
      <*> (getLarge . getPositive <$> arbitrary)

newtype Publisher = Publisher String
  deriving newtype (Show, Eq, Ord)
  deriving (Arbitrary) via PrintableString

data Book = Book
  { bookTitle :: String,
    bookAuthor :: Author,
    bookPublisher :: Publisher,
    bookYear :: Year,
    bookContent :: [Page]
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Book where
  arbitrary =
    Book
      <$> (getPrintableString <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (getNonEmpty <$> arbitrary)
