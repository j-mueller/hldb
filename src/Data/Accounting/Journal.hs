{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Accounting.Journal where

import           Control.Lens hiding ((...), (<|), (|>), singular)
import           Control.Monad.State (MonadState)
import           Data.AdditiveGroup hiding (Sum)
import           Data.Foldable
import           Data.FingerTree hiding (fromList, singleton)
import qualified Data.FingerTree as FT
import qualified Data.Map.Strict as M
import           Data.Semigroup
import           Data.Text (Text)
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
import           Data.Accounting.Transaction (Transaction)
import qualified Data.Accounting.Transaction as T
import           Text.Parsec
import           Text.Parsec.Text


data JournalMeasure = JournalMeasure { 
  _firstDay :: Option (Min Day),
  _lastDay  :: Option (Max Day),
  _accounts :: Accounts,
  _transactionCount :: Sum Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''JournalMeasure

instance Semigroup JournalMeasure where
  l <> r = JournalMeasure ds dm ac cc where
    ds = (l^.firstDay) <> (r^.firstDay)
    dm = (l^.lastDay)  <> (r^.lastDay)
    ac = (l^.accounts) <> (r^.accounts)
    cc = (l^.transactionCount) <> (r^.transactionCount)

instance Monoid JournalMeasure where
  mappend = (<>)
  mempty  = JournalMeasure mempty mempty mempty mempty

instance Measured JournalMeasure Transaction where
  measure = JournalMeasure 
    <$> Option . Just . Min . view T.date
    <*> Option . Just . Max . view T.date 
    <*> view T.accounts
    <*> const 1

data ReportingInterval = Day | Week | Month | Year
  deriving (Eq, Ord, Show, Enum, Bounded)

type JournalTree = FingerTree JournalMeasure Transaction

-- | Journal contains accounts, ordered by day (ascending; oldest first) and
-- can be split into arbitrary intervals for reporting
newtype Journal = Journal { _unJournal :: JournalTree }
  deriving (Eq, Ord, Show)

makeLenses ''Journal

instance Monoid Journal where
  mempty = Journal mempty
  mappend l (Journal r) = ft' where -- can't use the monoid instance of FingerTree because we need to respect the ordering
    ft' = foldl' (flip insert) l (toList r)

-- | Create a journal with a single entry
singleton :: Transaction -> Journal
singleton = Journal . FT.singleton

-- | Create a journal from a list of journal entries
fromList :: [Transaction] -> Journal
fromList = foldl' (flip insert) mempty 

-- | Split a journal into two parts, given a day: Transactions before that 
-- day, and transactions on and after that day
splitByDay :: Day -> JournalTree -> (JournalTree, JournalTree)
splitByDay dy t = (older, youngerEq) where
  pred1 v = (v^.lastDay) >= (Option $ Just $ Max dy)
  (older, youngerEq) = split pred1 t

-- | Insert an accounting entry into a journal
insert :: Transaction -> Journal -> Journal
insert txn (Journal jnl) = Journal ft' where
  d = view T.date txn
  (before, duringAndAfter) = splitByDay d jnl 
  ft' = (before |> txn) >< duringAndAfter

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
accountsFor from to (Journal jnl) = view accounts $ measure during where
  (_, duringAndAfter) = splitByDay from jnl 
  (during, _) = splitByDay (succ to) duringAndAfter

-- | Get balance since beginning of journal
balance :: Day -> Journal -> Accounts
balance d = view accounts . measure . fst . splitByDay (succ d) . view unJournal

journalMeasure :: Journal -> JournalMeasure
journalMeasure = measure . view unJournal 

totalBalance :: Journal -> Accounts
totalBalance = view accounts . journalMeasure

transactions :: Journal -> [Transaction]
transactions = toList . _unJournal

-- | Parse a journal
-- journalP :: (Stream s m Char, Monad m, MonadState (ParsingState (Currency Text)) m) => ParsecT s u m Journal
-- journalP = do
--   txns <- sepBy transactionP (skipMany1 endOfLine)
--   return $ fromList txns