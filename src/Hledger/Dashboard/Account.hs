{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Hledger.Dashboard.Account(
  Accounts(..),
  accounts,
  empty,
  account,
  -- * Combinators
  merge,
  -- * Parser
  accountP
) where

import           Control.Applicative hiding (empty)
import           Control.Lens hiding (children)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Char
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TreeMap (TreeMap(..), pathTo)
import           Hledger.Dashboard.Currency (Currency, defaultCurrencyP)
import           Hledger.Dashboard.ParsingState (
  ParsingState,
  defaultParsingState,
  lastCurrency
  )
import           Text.Parsec.Text
import           Text.Parsec hiding ((<|>), many)

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Control.Monad.State
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> import Test.QuickCheck hiding (scale)
-- >>> import Text.Parsec.Text
-- >>> import Text.Parsec.Prim
-- >>> import Hledger.Dashboard.Currency (Currency(..))
-- >>> :set -XScopedTypeVariables
-- >>> :set -XFlexibleInstances
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedStrings
-- >>> instance Arbitrary Text where arbitrary = T.pack <$> arbitrary
-- >>> instance Arbitrary (Currency Text) where arbitrary = Currency <$> fmap M.fromList arbitrary
-- >>> instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where arbitrary = TreeMap <$> arbitrary <*> (fmap M.fromList $ fmap (\l -> if length l > 2 then [] else l) arbitrary)
-- >>> instance Arbitrary Accounts where arbitrary = Accounts <$> arbitrary

-- | An account is a `TreeMap String Currency` and `Accounts` is a top-level
--   account.
newtype Accounts = Accounts { _accounts :: TreeMap Text (Currency Text) }
  deriving (Eq, Ord, Show)

makeLenses ''Accounts

instance Monoid Accounts where
  mempty = empty
  mappend = merge

instance AdditiveGroup Accounts where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Accounts . negateV . view accounts

-- | An empty set of `Accounts`
empty :: Accounts
empty = Accounts mempty

-- | Merge two `Accounts`'
--
-- prop> \(a :: Accounts) -> merge a empty == a
-- prop> \(a :: Accounts) -> merge empty a == a
-- prop> \((a, b, c) :: (Accounts, Accounts, Accounts)) -> (a `merge` b) `merge` c == a `merge` (b `merge` c)
-- prop> \((l, r) :: (Accounts, Accounts)) -> l `merge` r == r `merge` l
merge :: Accounts -> Accounts -> Accounts
merge l r = Accounts $ mappend (view accounts l) (view accounts r)

-- | Create an `Accounts` object with a single top-level account
account :: TreeMap Text (Currency Text) -> Accounts
account = Accounts

-- | Parse an `Accounts` value. Each node in the result will have at most one
-- child.
--
accountP :: (Monad m, Stream s m Char, MonadState ParsingState m) => ParsecT s u m Accounts
accountP = do
  accName <- accountNameP <?> "account name"
  _ <- many (satisfy isSpace) <?> "space between account name and currency"
  curr <- defaultCurrencyP <?> "currency"
  return $ Accounts $ pathTo accName curr

-- | Parse the name of an account in a hierarchy.
--
-- >>> let acc = T.pack "Expenses:Cash"
-- >>> evalState (runParserT accountNameP () "" acc) defaultParsingState
-- Right ["Expenses","Cash"]
accountNameP :: (Monad m, Stream s m Char) => ParsecT s u m [Text]
accountNameP = fmap (T.splitOn ":") p where
  p = T.pack <$> theChars <?> "account name"
  theChars = (:) <$> letter <*> rest
  rest = manyTill anyChar end <?> "rest of account name"
  end = string "  " <|> string "\r" <|> string "\r\n" <|> (fmap (const "")  eof )
