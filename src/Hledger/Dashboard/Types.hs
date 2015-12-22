{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Hledger.Dashboard.Types where

import           Control.Comonad.Env
import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace.Point
import           Data.AffineSpace
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Set as S
import           Data.Time.Clock
import           Data.VectorSpace

type Time = Integer

data Posting = Posting {
  _source :: String,
  _target :: String,
  _amount :: Integer,
  _dateTime :: UTCTime
} deriving (Eq, Ord)

makeLenses ''Posting

invert :: Posting -> Posting
invert pt = pt
  & source.~(pt^.target)
  & target.~(pt^.source)

newtype Postings = Postings { _unPostings :: S.Set Posting }

makeLenses ''Postings

instance Monoid Postings where
  mempty = Postings mempty
  mappend l r = Postings $ l^.unPostings <> r^.unPostings

newtype IncomeStatement = IncomeStatement { _unIS :: Env Postings  NominalDiffTime }

makeLenses ''IncomeStatement

postings :: Lens' IncomeStatement Postings
postings = lens t f where
  t is = is^.unIS.(to ask)
  f p = undefined

diffTime :: Lens' IncomeStatement NominalDiffTime
diffTime = lens t f where
  t is = is^.unIS.(to extract)
  f = undefined

instance AdditiveGroup IncomeStatement where
  zeroV = IncomeStatement $ env mempty 0
  l ^+^ r = IncomeStatement $ env postings' time' where
    postings' = l^.postings <> r^.postings
    time'     = l^.diffTime +  r^.diffTime
  negateV p = IncomeStatement $ env postings' time' where
    postings' = p^.postings
    time' = negate $ p^.diffTime

type BalanceSheet = Point IncomeStatement

--
-- instance VectorSpace IncomeStatement where
--   type Scalar IncomeStatement = NominalDiffTime
--   (*^) s = over diffTime ((*) s) -- not good (see below)

-- Main idea: The income/expenses between two points in time should be the
-- difference between the two Points!
-- Problem: How to reconcile subtraction with the attempt to have ^+^ mean the
-- UNION of two sets of postings (no duplicates)?
-- Solutions
-- 1) Maintain two separate sets for included/excluded postings?
-- 2) Use DiffTime in postings, instead of UTC Time? (ie postings are like a
--    linked list)
-- 3) Use mostly additive groups + maybe affine spaces? (Need Diff instance)
