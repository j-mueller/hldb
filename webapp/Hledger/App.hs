{-# LANGUAGE TemplateHaskell #-}
{- Implementation of ledger webapp -}
module Hledger.App where

import Control.Lens
import Data.Text (Text)

import Data.Accounting.Journal(
  ReportingInterval(..),
  Journal,
  firstDay,
  accountsFor,
  balance
  )

data AppState = AppState{
  _journal :: Journal
}

makeLenses ''AppState
