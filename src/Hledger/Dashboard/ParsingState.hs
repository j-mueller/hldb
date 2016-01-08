{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Dashboard.ParsingState(
  ParsingState,
  defaultParsingState,
  -- * Lenses
  defaultDay,
  lastCurrencySymbol,
  runningTotal
) where

import           Control.Lens hiding (children)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time.Calendar (Day, fromGregorian)

-- | Data that is needed during parsing
-- Parameterised over the currency type in order to avoid cyclic references in
-- source files.
--
data ParsingState c = ParsingState{
  _lastCurrencySymbol :: Text, -- ^ Last seen currency symbol. Used as a default value if no currency symbol is found.
  _defaultDay :: Day, -- ^ Default value for dates when parsing transactions.
  _runningTotal :: c -- ^ Running total of the transaction. Used for balancing
}
  deriving (Eq, Ord, Show)

makeLenses ''ParsingState

defaultParsingState :: Monoid c => ParsingState c
defaultParsingState = ParsingState cur dt mempty where
  cur = ""
  dt = fromGregorian 1970 1 1

instance Monoid c => Monoid (ParsingState c) where
  mempty = defaultParsingState
  mappend _ r = r
