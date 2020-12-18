{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module CommandTest where

import Protocol.Command
    ( ReturnType(FireAndForget), Command, cast )
import Protocol.UnboundedMessageBox(InBoxConfig(UnboundedMessageBox))
import Test.Tasty as Tasty ( testGroup, TestTree )
import Test.Tasty.QuickCheck
    ( ioProperty,
      Arbitrary(arbitrary),
      Large(getLarge),
      NonEmptyList(getNonEmpty),
      Positive(..),
      PrintableString(..),
      Property,
      testProperty )
import UnliftIO ( conc, runConc )
import Protocol.MessageBoxClass
    ( IsMessageBox(newOutBox, newInBox) )
import Data.Semigroup
import Data.Functor

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Protocol.Command"
    [ testProperty "all books that many donors concurrently donate into the book store end up in the bookstore" $
        donateAllBooks_prop
    ]

donateAllBooks_prop :: [(Donor, Book)] -> Property
donateAllBooks_prop donorsAndBooks = ioProperty $ do
  bookStoreIn <- newInBox UnboundedMessageBox
  bookStoreOut <- newOutBox bookStoreIn
  runConc
    ( foldMap
        ( \(donor, book) ->
            conc (cast bookStoreOut (Donate donor book) $> All True)
        )
        donorsAndBooks
    <> pure (All True))

data BookStore

data instance Command BookStore _ where
  Donate :: Donor -> Book -> Command BookStore 'FireAndForget

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
    donorId :: Integer
  }
  deriving stock (Show, Eq, Ord)

instance Arbitrary Donor where
  arbitrary =
    Donor
      <$> arbitrary
      <*> (getLarge . getPositive <$> arbitrary)

data Author = Author
  { authorName :: PersonName,
    authorId :: Integer
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
