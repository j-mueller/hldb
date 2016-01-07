{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Dashboard.ParsingState(
  ParsingState,
  defaultParsingState,
  -- * Lenses
  lastCurrency,
  defaultDay
) where

import           Control.Lens hiding (children)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time.Calendar (Day, fromGregorian)

-- | Data that is needed during parsing
data ParsingState = ParsingState{
  _lastCurrency :: Text,
  _defaultDay :: Day
}
  deriving (Eq, Ord, Show)

makeLenses ''ParsingState

defaultParsingState :: ParsingState
defaultParsingState = ParsingState cur dt where
  cur = ""
  dt = fromGregorian 1970 1 1

instance Monoid ParsingState where
  mempty = defaultParsingState
  mappend _ r = r
