{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Dashboard.Transaction(
  Transaction(..),
  date,
  accounts
) where

import           Control.Lens
import           Control.Monad.Identity
import           Data.Attoparsec.Text
import           Data.Time.Calendar (Day)

import           Hledger.Dashboard.Account(Accounts)

-- | A transaction consists of a date, accounts and some metadata
data Transaction f = Transaction{
  _date :: f Day,
  _accounts :: f Accounts
  -- TODO: Add fields for cleared/pending transactions
  -- TODO: Add tags
}

makeLenses ''Transaction

-- | Parse a transaction
--
-- @
-- 2015\/12\/12 gift
--     Expenses:Gifts  €10
--     Assets:Cash
-- @
transactionP :: Parser (Transaction Identity)
transactionP = undefined
