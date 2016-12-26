{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Accounting.Currency(
  Currency(..),
  -- * Constructors
  empty,
  currency,
  Currency(..),
  -- * Operations
  add,
  scale,
  plus,
  invert,
  toList,
  -- * Other
  mapCurrencies,
  nonZero
) where

import           Control.Applicative hiding (empty, optional)
import           Control.Lens hiding ((...), singular)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Char (isDigit, isPrint, isSpace)
import           Data.List (intercalate)
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Ratio
import           Data.Semigroup hiding (option)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.VectorSpace
import           Text.Parsec hiding ((<|>), many)
import           Text.Parsec.Combinator
import           Text.Parsec.Text
import           Text.Read (readEither)

-- | Values with currencies.
newtype Currency a = Currency { _values :: M.Map a Rational }
  deriving (Eq, Ord)

makeLenses ''Currency

nonZero :: Ord a => M.Map a Rational -> M.Map a Rational
nonZero = M.filter (not . (==) 0)

mapCurrencies :: Ord b => (a -> b) -> Currency a -> Currency b
mapCurrencies f = Currency . M.mapKeys f . view values

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Control.Monad.State
-- >>> import Text.Parsec.Text
-- >>> import Text.Parsec.Prim
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> :set -XScopedTypeVariables
-- >>> :set -XFlexibleInstances
-- >>> :set -XOverloadedStrings
-- >>> let parseOnly p s = evalState (runParserT p () "" (T.pack s)) defaultParsingState

instance Ord a => Semigroup (Currency a) where
  (<>) = mappend

-- | `Currency` is a `Monoid` where `<>` is `plus` and `mempty` is `empty`
instance Ord a => Monoid (Currency a) where
  mempty = empty
  mappend = plus

-- | `Currency` is an `AdditiveGroup` where `zeroV` and `^+^` are the monoid
--   operations and `negateV` is `invert`
instance Ord a => AdditiveGroup (Currency a) where
  zeroV = empty
  (^+^) = plus
  negateV = invert

instance (Ord a, Show a) => Show (Currency a) where
  show = show . toList

-- | `Currency` is a `VectorSpace` with `scale`
instance Ord a => VectorSpace (Currency a) where
  type Scalar (Currency a) = Rational
  (*^) = scale

-- | Create a `Currency` with a single value
--
-- >>> currency 1 "EUR"
-- [("EUR",1 % 1)]
currency :: Rational -> Text -> Currency Text
currency r = Currency . nonZero . flip M.singleton r

-- | Add an amount to a `Currency`
--
-- >>> add 1 "EUR" $ currency 1 "EUR"
-- [("EUR",2 % 1)]
-- >>> add 1 "GBP" $ currency 1 "EUR"
-- [("EUR",1 % 1),("GBP",1 % 1)]
-- >>> add (-1) "GBP" $ currency 1 "GBP"
-- []
add :: Ord a => Rational -> a -> Currency a -> Currency a
add r s = Currency . nonZero . M.insertWith (+) s r . view values

-- | Scale a currency by a factor
--
-- >>> scale 2 $ currency 1 "EUR"
-- [("EUR",2 % 1)]
-- >>> scale 2.5 $ add 1 "GBP" $ currency 5 "EUR"
-- [("EUR",25 % 2),("GBP",5 % 2)]
-- >>> scale 0 $ currency 1 "EUR"
-- []
scale :: Ord a => Rational -> Currency a -> Currency a
scale f = Currency . nonZero . M.map ((*) f) . view values

-- | Empty `Currency` with no values.
--
-- >>> empty
-- []
empty :: Currency a
empty = Currency M.empty

-- | Combine two `Currency`s by adding their values.
plus :: Ord a => Currency a -> Currency a -> Currency a
plus l r = Currency $ nonZero $ M.unionWith (+) (l^.values) (r^.values)

-- | Invert the values of a `Currency` by multiplying them with -1.
invert :: Ord a => Currency a -> Currency a
invert = Currency . fmap negate . view values

-- | Get a list of the values in this `Currency`
toList :: Ord a => Currency a -> [(a, Rational)]
toList = M.toList . view values