{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Accounting.Journal where

import           Control.Lens hiding ((...), (<|), singular)
import           Data.AdditiveGroup
import           Data.Foldable
import           Data.FingerTree hiding (singleton)
import qualified Data.FingerTree as FT
import qualified Data.Map.Strict as M
import           Data.Semigroup
import           Data.Time.Calendar (
  Day,
  addDays,
  addGregorianMonthsClip,
  addGregorianYearsClip,
  fromGregorian,
  toGregorian)
import           Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import           Data.Accounting.Account (Accounts)
import           Data.Accounting.Transaction (Transaction)
import qualified Data.Accounting.Transaction as T
import           Text.Parsec
import           Text.Parsec.Text


data JournalMeasure = JournalMeasure { 
  _firstDay :: Min Day,
  _accounts :: Accounts
  }

makeLenses ''JournalMeasure

instance Semigroup JournalMeasure where
  l <> r = JournalMeasure ds ac where
    ds = (l^.firstDay) <> (r^.firstDay)
    ac = (l^.accounts) <> (r^.accounts)

instance Measured (Option JournalMeasure) Transaction where
  measure txn = Option $ Just $ JournalMeasure (Min $ view T.date txn) (view T.accounts txn)

data ReportingInterval = Day | Week | Month | Year
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Journal contains accounts, ordered by day (descending; youngest first) and
-- can be split into arbitrary intervals for reporting
newtype Journal = Journal { _unJournal :: FingerTree (Option JournalMeasure) Transaction }

makeLenses ''Journal

instance Monoid Journal where
  mempty = Journal mempty
  mappend (Journal l) (Journal r) = Journal (l `mappend` r)

-- TODO: instance Monoid Journal

-- | Create a journal with a single entry
singleton :: Transaction -> Journal
singleton = Journal . FT.singleton

-- | Insert an accounting entry into a journal
insert :: Transaction -> Journal -> Journal
insert txn (Journal ft) = Journal ft' where
  d = view T.date txn
  ft' = after `mappend` rest'
  rest' = txn <| rest
  (after, rest) = split ((>=) (Option $ Just $ Min d) . (fmap (view firstDay))) ft

-- | Get the first day of the interval containing the given day
-- | If the interval is `Day` then `begin` and `end` both evaluate to `id`
begin :: ReportingInterval -> Day -> Day
begin i d = case i of
  Day -> d
  Week -> fromWeekDate y w 1 where
    (y, w, _) = toWeekDate d
  Month -> fromGregorian y m 1 where
    (y, m, _) = toGregorian d
  Year -> fromGregorian y 1 1 where
    (y, _, _) = toGregorian d

-- | Get the last day of the interval containing the given day
-- | If the interval is `Day` then `begin` and `end` both evaluate to `id`
end :: ReportingInterval -> Day -> Day
end i d = case i of
  Day -> d
  Week -> pred $ fromWeekDate y w 1 where
    (y, w, _) = toWeekDate d'
    d' = addDays 7 d
  Month -> pred $ fromGregorian y m 1 where
    (y, m, _) = toGregorian $ addGregorianMonthsClip 1 d
  Year -> pred $ fromGregorian y 1 1 where
    (y, _, _) = toGregorian $ addGregorianYearsClip 1 d

-- | Get accounts for an interval
accountsFor :: Day -> Day -> Journal -> Accounts
accountsFor from to (Journal ft) = maybe mempty (view accounts) $ getOption $ measure during where
  (after, rest) = split ((>=) (Option $ Just $ Min from) . (fmap (view firstDay))) ft
  (during, _)   = split ((<=) (Option $ Just $ Min to) . (fmap (view firstDay))) ft

-- | Get balance since beginning of journal
balance :: Day -> Journal -> Accounts
balance d = maybe mempty (view accounts) . getOption . measure . takeUntil ((<=) (Option $ Just $ Min $ succ d) . (fmap (view firstDay))) . view unJournal
