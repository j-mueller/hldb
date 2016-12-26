{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Accounting.Transaction(
  Transaction(..),
  -- * Accessors
  date,
  description,
  accounts,
  -- * Operators
  addAccounts
) where

import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import           Text.Parsec
import           Text.Parsec.Text

import           Data.Accounting.Account (Accounts)
import           Data.Accounting.Currency (Currency)

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

addAccounts :: Accounts -> Transaction -> Transaction
addAccounts a t = t & accounts <>~ a