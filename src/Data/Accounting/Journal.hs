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
import           Data.Accounting.Currency (Currency)
import           Numeric.Interval hiding (singleton)
import qualified Numeric.Interval as I

data JournalMeasure = JournalMeasure { 
  _days :: Interval Day,
  _accounts :: Accounts
  }

makeLenses ''JournalMeasure

instance Semigroup JournalMeasure where
  l <> r = JournalMeasure ds ac where
    ds = (l^.days) `hull` (r^.days)
    ac = (l^.accounts) <> (r^.accounts)

instance Measured (Option JournalMeasure) (Day, Accounts) where
  measure (d, ac) = Option $ Just $ JournalMeasure (I.singleton d) ac

containsDay :: Day -> Option JournalMeasure -> Bool
containsDay d = maybe False id . fmap (I.elem d . view days) . getOption 

-- | Check if a journal measure is contained in an interval
isContainedIn :: Interval Day -> Option JournalMeasure -> Bool
isContainedIn i = maybe False id . fmap (flip I.contains i . view days) . getOption

endsBefore :: Day -> Option JournalMeasure -> Bool
endsBefore d = maybe False id . fmap ((<) d . sup . view days) . getOption

data ReportingInterval = Day | Week | Month | Year
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Journal contains accounts, ordered by day and can be split into
-- arbitrary intervals for reporting
newtype Journal = Journal { _unJournal :: FingerTree (Option JournalMeasure) (Day, Accounts) }

makeLenses ''Journal

-- TODO: instance Monoid Journal

-- | Create a journal with a single entry
singleton :: Day -> Accounts -> Journal
singleton d a = Journal $ FT.singleton (d, a)

-- | Insert an accounting entry into a journal
insert :: Day -> Accounts -> Journal -> Journal
insert d a (Journal ft) = Journal ft' where
  ft' = before `mappend` during' `mappend` after
  during' = (d, a) <| during
  (before, (during, after)) = fmap (split (not . containsDay d)) $ split (containsDay d) ft

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
accountsFor :: Interval Day -> Journal -> Accounts
accountsFor i (Journal ft) = maybe mempty (view accounts) $ getOption $ measure during where
  (_, (during, _)) = fmap (split (not . isContainedIn i)) $ split (isContainedIn i) ft

-- | Get balance since beginning of journal
balance :: Day -> Journal -> Accounts
balance d = maybe mempty (view accounts) . getOption . measure . takeUntil (endsBefore $ succ d) . view unJournal
