{-# LANGUAGE TemplateHaskell #-}
module Hledger.Dashboard.Currency(
  Currency,
  currency,
  add
) where

import           Control.Lens hiding ((...), singular)
import           Data.AdditiveGroup
import           Data.List (intercalate)
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Ratio

-- | Values with currencies.
newtype Currency = Currency { _values :: M.Map String Rational }
  deriving (Eq, Ord)

makeLenses ''Currency

nonZero :: M.Map String Rational -> M.Map String Rational
nonZero = M.filter (not . (==) 0)

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Currency where arbitrary = Currency <$> fmap M.fromList arbitrary
-- >>> :set -XScopedTypeVariables

-- | `Currency` is a `Monoid` under addition and empty set of amounts.
--
-- The following properties hold:
--
-- prop> \(r :: Currency) -> r <> mempty == r
-- prop> \(r :: Currency) -> mempty <> r == r
-- prop> \((a, b, c) :: (Currency, Currency, Currency)) -> (a <> b) <> c == a <> (b <> c)
instance Monoid Currency where
  -- | Currency without any values
  mempty = Currency M.empty

  -- | Combine two `Currency`s by adding their values
  mappend l r = Currency $ nonZero $ M.unionWith (+) (l^.values) (r^.values)

-- | `Currency` is an additive group where `zeroV` and `^+^` are the monoid op-
--   erations and `negateV` is negating the amounts
--
-- The following properties hold:
--
-- prop> \(r :: Currency) -> r ^-^ r == zeroV
-- prop> \((l, r) :: (Currency, Currency)) -> l ^+^ r == r ^+^ l
instance AdditiveGroup Currency where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Currency . fmap negate . view values

instance Show Currency where
  show c = intercalate ", " $ fmap (\(k, v) -> (showF v) ++ " " ++ k) $ M.assocs $ view values c where
    showF :: Rational -> String
    showF v' = show ((a / b) :: Double) where (a, b) = (fromInteger $ numerator v', fromInteger $ denominator v')

-- | Create a `Currency` with a single value
--
-- >>> currency 1 "EUR"
-- 1.0 EUR
currency :: Rational -> String -> Currency
currency r s = Currency $ nonZero $ M.fromList [(s, r)]

-- | Add an amount to a `Currency`
--
-- >>> add 1 "EUR" $ currency 1 "EUR"
-- 2.0 EUR
-- >>> add 1 "GBP" $ currency 1 "EUR"
-- 1.0 EUR, 1.0 GBP
-- >>> add (-1) "GBP" $ currency 1 "GBP"
-- <BLANKLINE>
add :: Rational -> String -> Currency -> Currency
add r s = Currency . nonZero . M.insertWith (+) s r . view values
