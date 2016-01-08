{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Hledger.Dashboard.Transaction(
  Transaction(..),
  date,
  accounts,
  transactionP
) where

import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian)
import           Text.Parsec
import           Text.Parsec.Text
import           Text.Read (readEither)

import           Hledger.Dashboard.Account (Accounts, accountP)
import           Hledger.Dashboard.ParsingState (
  ParsingState,
  defaultParsingState)

-- | A transaction consists of a date, accounts and some metadata
data Transaction = Transaction{
  _date :: Day,
  _description :: Text,
  _accounts :: Accounts
  -- TODO: Add fields for cleared/pending transactions
  -- TODO: Add tags
}
  deriving (Eq, Ord, Show)

makeLenses ''Transaction

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Control.Monad.State
-- >>> import Text.Parsec.Text
-- >>> import Text.Parsec.Prim
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> :set -XScopedTypeVariables
-- >>> :set -XFlexibleInstances
-- >>> let parseOnly p s = evalState (runParserT p () "" (T.pack s)) defaultParsingState


-- | Parse a transaction
--
-- @
-- 2015\/12\/12 beer
--     Expenses:Gifts  €10
--     Assets:Cash
-- @
--
transactionP :: (Stream s m Char, Monad m, MonadState ParsingState m) => ParsecT s u m Transaction
transactionP = do
  d <- dateP <?> "date"
  _ <- spaces
  description <- fmap T.pack (manyTill anyChar endOfLine) <?> "description"
  accs <- sepEndBy1 (spaces >> accountP) endOfLine <?> "postings"
  return $ Transaction d description $ fold accs

-- | Parse a date
--
-- >>> parseOnly dateP "2015/12/12"
-- Right 2015-12-12
dateP :: (Stream s m Char, Monad m, MonadState ParsingState m) => ParsecT s u m Day
dateP = do
  let toI p = p >>= either fail return . readEither
  year <- toI (count 4 digit) <?> "year"
  _ <- char '/' <?> "year/month separator"
  month <- toI (count 2 digit) <?> "month"
  _ <- char '/' <?> "month/day separator"
  day <- toI (count 2 digit) <?> "day"
  return $ fromGregorian year month day
