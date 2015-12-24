{-# LANGUAGE TemplateHaskell #-}
module Hledger.Dashboard.Types where

import           Control.Lens
import           Data.AdditiveGroup
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Time.Calendar

newtype Accounts = Accounts { _values :: M.Map String Rational }
  deriving (Eq, Ord)

makeLenses ''Accounts

instance Monoid Accounts where
  mempty = Accounts M.empty
  mappend l r = Accounts $ M.unionWith (+) (l^.values) (r^.values)

instance AdditiveGroup Accounts where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Accounts . fmap negate . view values

data ReportingInterval = Day | Week | Month | Quarter | Year
  deriving (Eq, Ord, Show, Enum)

-- | Journal contains accounts for various reporting periods
newtype Journal = Journal {
  _intervals :: M.Map (Day, ReportingInterval) Accounts
}

makeLenses ''Journal
