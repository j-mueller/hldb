{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Hledger.Dashboard.Currency(
  Currency(..),
  -- * Constructors
  empty,
  currency,
  -- * Operations
  add,
  scale,
  plus,
  invert,
  toList,
  -- * Parser
  currencyP,
  defaultCurrencyP
) where

import           Control.Applicative hiding (empty)
import           Control.Lens hiding ((...), singular)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Char (isDigit, isPrint, isSpace)
import           Data.List (intercalate)
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.VectorSpace
import           Text.Parsec hiding ((<|>), many)
import           Text.Parsec.Combinator
import           Text.Parsec.Text
import           Text.Read (readMaybe)

import Hledger.Dashboard.ParsingState

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
-- >>> import Test.QuickCheck hiding (scale)
-- >>> :set -XScopedTypeVariables
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleInstances
-- >>> instance Arbitrary (Currency Text) where arbitrary = Currency <$> (fmap M.fromList $ fmap (fmap (\p -> (T.pack $ fst p, snd p))) $ arbitrary)
-- >>> let parseOnly p s = evalState (runParserT p () "" s) defaultParsingState

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
-- prop> \(r :: Currency Text) -> scale 0 r == empty
-- prop> \(r :: Currency Text) -> scale 1 r == r
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
--
-- prop> \(r :: Currency Text) -> r `plus` mempty == r
-- prop> \(r :: Currency Text) -> mempty `plus` r == r
-- prop> \((a, b, c) :: (Currency Text, Currency Text, Currency Text)) -> (a `plus` b) `plus` c == a `plus` (b `plus` c)
-- prop> \((l, r) :: (Currency Text, Currency Text)) -> l `plus` r == r `plus` l
plus :: Ord a => Currency a -> Currency a -> Currency a
plus l r = Currency $ nonZero $ M.unionWith (+) (l^.values) (r^.values)

-- | Invert the values of a `Currency` by multiplying them with -1.
--
-- prop> \(r :: Currency Text) -> (invert $ invert r) == r
-- prop> \(r :: Currency Text) -> r `plus` (invert r) == empty
invert :: Ord a => Currency a -> Currency a
invert = Currency . fmap negate . view values

-- | Get a list of the values in this `Currency`
toList :: Ord a => Currency a -> [(a, Rational)]
toList = M.toList . view values

-- | Parse a `Currency` from `Text`. The return value has a single
-- currency-amount pair. If no currency amount is found, then the currency will
-- be `Nothing`.
-- >>> parseOnly currencyP $ T.pack "1 EUR"
-- Right [(Just "EUR",1 % 1)]
-- >>> parseOnly currencyP $ T.pack "0.5"
-- Right [(Nothing,1 % 2)]
-- >>> parseOnly currencyP $ T.pack "GBP 12.0"
-- Right [(Just "GBP",12 % 1)]
-- >>> parseOnly currencyP $ T. pack "GBP38.11"
-- Right [(Just "GBP",3811 % 100)]
currencyP :: (Monad m, MonadState ParsingState m, Stream s m Char) => ParsecT s u m (Currency (Maybe Text))
currencyP = parse <?> "currencyP" where
  parse = (try leftSymbolCurrencyP  <?> "leftSymbolCurrencyP")
      <|> (try rightSymbolCurrencyP <?> "rightSymbolCurrencyP")
      <|> (noSymbolCurrencyP    <?> "noSymbolCurrencyP")
  currency' r = Currency . nonZero . flip M.singleton r
  leftSymbolCurrencyP  = do
    sgn <- signP
    s   <- currencySymbol
    _  <- many (satisfy isSpace)
    amt <- fmap sgn $ rational
    return $ currency' amt $ Just s
  rightSymbolCurrencyP = do
    amt <- rational
    _  <- many (satisfy isSpace)
    s   <- currencySymbol
    return $ currency' amt $ Just s
  noSymbolCurrencyP = do
    sgn <- signP
    r <- fmap sgn $ rational
    return $ currency' r $ Nothing

-- | Parse a `Currency` with a default currency value
--
-- >>> parseOnly defaultCurrencyP $ T.pack "-10.0"
-- Right [("EUR",(-10) % 1)]
defaultCurrencyP :: (Monad m, MonadState ParsingState m, Stream s m Char) => ParsecT s u m (Currency Text)
defaultCurrencyP = do
  c <- gets $ view lastCurrency
  let applyDefault = mapCurrencies (maybe c id)
  fmap applyDefault currencyP

-- | Parse a currency symbol
currencySymbol :: (Monad m, MonadState ParsingState m, Stream s m Char) => ParsecT s u m Text
currencySymbol = fmap T.pack p where
  p = many1 (satisfy cond) <?> "currency symbol"
  cond c = isPrint c && (not $ isSpace c) && (not $ isDigit c)

-- | Parse a sign (+/-) to a function,  `id` for optional '+' and `negate`
--   for '-'
signP :: (Monad m, Stream s m Char) => ParsecT s u m (Rational -> Rational)
signP = try m <|> p where
  m = char '-' >> return negate
  p = option id $ char '+' >> return id

rational :: (Stream s m Char, Monad m) => ParsecT s u m Rational
rational = do
  s <- signP
  let cond c = isDigit c || c == '.'
  chars <- many (satisfy cond) <?> "rational"
  maybe (fail "") (return . s) $ readMaybe chars
