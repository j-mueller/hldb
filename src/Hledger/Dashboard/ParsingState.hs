{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Dashboard.ParsingState(
  ParsingState,
  defaultParsingState,
  -- * Lenses
  lastCurrency
) where

import           Control.Lens hiding (children)
import           Data.Monoid
import           Data.Text (Text)

-- | Data that is needed during parsing
data ParsingState = ParsingState{
  _lastCurrency :: Text
}
  deriving (Eq, Ord, Show)

makeLenses ''ParsingState

defaultParsingState :: ParsingState
defaultParsingState = ParsingState ""

instance Monoid ParsingState where
  mempty = defaultParsingState
  mappend _ r = r
