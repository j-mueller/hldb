{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Hledger.Dashboard.Currency(
  Currency,
  -- * Constructors
  empty,
  currency,
  -- * Operations
  add,
  scale,
  plus,
  invert
) where

import           Control.Lens hiding ((...), singular)
import           Data.AdditiveGroup
import           Data.List (intercalate)
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Ratio
import           Data.VectorSpace

-- | Values with currencies.
newtype Currency = Currency { _values :: M.Map String Rational }
  deriving (Eq, Ord)

makeLenses ''Currency

nonZero :: M.Map String Rational -> M.Map String Rational
nonZero = M.filter (not . (==) 0)

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Test.QuickCheck hiding (scale)
-- >>> instance Arbitrary Currency where arbitrary = Currency <$> fmap M.fromList arbitrary
-- >>> :set -XScopedTypeVariables

-- | `Currency` is a `Monoid` where `<>` is `plus` and `mempty` is `empty`
--
instance Monoid Currency where
  mempty = empty
  mappend = plus

-- | `Currency` is an additive group where `zeroV` and `^+^` are the monoid op-
--   erations and `negateV` is `invert`
--
instance AdditiveGroup Currency where
  zeroV = empty
  (^+^) = plus
  negateV = invert

instance Show Currency where
  show c = intercalate ", " $ fmap (\(k, v) -> (showF v) ++ " " ++ k) $ M.assocs $ view values c where
    showF :: Rational -> String
    showF v' = show ((a / b) :: Double) where (a, b) = (fromInteger $ numerator v', fromInteger $ denominator v')

-- | `Currency` is a `VectorSpace` with `scale`
--
-- prop> \(r :: Currency) -> 0 *^ r == zeroV
-- prop> \(r :: Currency) -> 1 *^ r == r
instance VectorSpace Currency where
  type Scalar Currency = Rational
  (*^) = scale

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

-- | Scale a currency by a factor
--
-- >>> scale 2 $ currency 1 "EUR"
-- 2.0 EUR
-- >>> scale 2.5 $ add 1 "GBP" $ currency 5 "EUR"
-- 12.5 EUR, 2.5 GBP
-- >>> scale 0 $ currency 1 "EUR"
-- <BLANKLINE>
scale :: Rational -> Currency -> Currency
scale f = Currency . nonZero . M.map ((*) f) . view values

-- | Empty `Currency` with no values.
--
-- >>> empty
-- <BLANKLINE>
empty :: Currency
empty = Currency M.empty

-- | Combine two `Currency`s by adding their values.
--
-- prop> \(r :: Currency) -> r `plus` mempty == r
-- prop> \(r :: Currency) -> mempty `plus` r == r
-- prop> \((a, b, c) :: (Currency, Currency, Currency)) -> (a `plus` b) `plus` c == a `plus` (b `plus` c)
-- prop> \((l, r) :: (Currency, Currency)) -> l `plus` r == r `plus` l
plus :: Currency -> Currency -> Currency
plus l r = Currency $ nonZero $ M.unionWith (+) (l^.values) (r^.values)

-- | Invert the values of a `Currency` by multiplying them with -1.
--
-- prop> \(r :: Currency) -> (invert $ invert r) == r
-- prop> \(r :: Currency) -> r `plus` (invert r) == empty
invert :: Currency -> Currency
invert = Currency . fmap negate . view values
