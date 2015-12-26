{-# LANGUAGE TemplateHaskell #-}
module Hledger.Dashboard.Types where

import           Control.Lens hiding ((...), singular)
import           Data.AdditiveGroup
import           Data.Foldable
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Time.Calendar
import           Numeric.Interval

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
  deriving (Eq, Ord, Show, Enum, Bounded)

enumerate :: (Enum a, Bounded a) => [a]
enumerate = enumFromTo minBound maxBound

-- | Journal contains accounts for various reporting periods
data Journal = Journal {
  _intervals :: M.Map (Day, ReportingInterval) Accounts,
  _firstDay :: Maybe Day
}

makeLenses ''Journal

-- | Get the first day of the interval containing the given day
-- | If the interval is `Day` then `begin` and `end` both evaluate to `id`
begin :: ReportingInterval -> Day -> Day
begin i = case i of
  Day -> id
  Week -> undefined

-- | Get the last day of the interval containing the given day
-- | If the interval is `Day` then `begin` and `end` both evaluate to `id`
end :: ReportingInterval -> Day -> Day
end i = case i of
  Day -> id
  _ -> undefined

-- | Get all intervals starting at a given date
startingAt :: Day -> [ReportingInterval]
startingAt d = takeWhile ((==) d . flip begin d) enumerate

breakDown :: Interval Day -> [(Day, ReportingInterval)]
breakDown i = current : rest where
  f = inf i
  t = sup i
  nextStart = succ $ uncurry (flip end) current
  current = (f, maximum $ filter endsBefore $ startingAt f)
  endsBefore rpi = (end rpi f) <= t
  rest = case singular i of
    True  -> []
    False -> breakDown $ nextStart ... t

-- | Get accounts for an interval
accountsFor :: Interval Day -> Journal -> Accounts
accountsFor i j = fold accts where
  start = maybe fd (min fd) $ view firstDay j
  fd = inf i
  lookp t = M.findWithDefault mempty t $ view intervals j
  accts = map lookp $ breakDown i

-- | Get balance since beginning of journal
balance :: Day -> Journal -> Accounts
balance d j = accountsFor (start ... d) j where
  start = maybe d id $ view firstDay j
