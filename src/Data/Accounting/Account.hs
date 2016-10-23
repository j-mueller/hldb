{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Accounting.Account(
  Accounts(..),
  accounts,
  balance,
  empty,
  account,
  -- * Combinators
  merge,
  -- * Parser
  accountP,
  accountNameP
) where

import           Control.Applicative hiding (empty)
import           Control.Lens hiding (children)
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Char
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TreeMap (TreeMap(..), pathTo)
import           Data.Accounting.Currency (Currency, balancingCurrencyP)
import           Data.Accounting.ParsingState (
  ParsingState,
  defaultParsingState,
  runningTotal
  )
import           Text.Parsec.Text
import           Text.Parsec hiding ((<|>), many)

-- | An account is a `TreeMap String Currency` and `Accounts` is a top-level
--   account.
newtype Accounts = Accounts { _accounts :: TreeMap Text (Currency Text) }
  deriving (Eq, Ord, Show)

makeLenses ''Accounts

instance Semigroup Accounts where
  (<>) = merge

instance Monoid Accounts where
  mempty = empty
  mappend = merge

instance AdditiveGroup Accounts where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Accounts . negateV . view accounts

-- | An empty set of `Accounts`
empty :: Accounts
empty = Accounts mempty

-- | Get the total balance of an `Accounts` value
balance :: Accounts -> Currency Text
balance = fold . view accounts

-- | Merge two `Accounts`. `merge` is associative and commutative and `empty`
-- acts as its unit.
merge :: Accounts -> Accounts -> Accounts
merge l r = Accounts $ mappend (view accounts l) (view accounts r)

-- | Create an `Accounts` object with a single top-level account
account :: TreeMap Text (Currency Text) -> Accounts
account = Accounts

-- | Parse an `Accounts` value. Each node in the result will have at most one
-- child.
--
accountP :: (Monad m, Stream s m Char, MonadState (ParsingState (Currency Text)) m) => ParsecT s u m Accounts
accountP = do
  accName <- accountNameP <?> "account name"
  _       <- spaces <?> "space between account name and currency"
  curr    <- balancingCurrencyP <?> "currency"
  let res = Accounts $ pathTo accName curr
  return res

-- | Parse the name of an account in a hierarchy. The `:` symbol is used as a
-- separator. Example: `Expenses:Gifts` results in `["Expenses", "Gifts"]`.
--
accountNameP :: (Monad m, Stream s m Char) => ParsecT s u m [Text]
accountNameP = fmap (T.splitOn ":" . T.filter (not . (==) '\r') . T.filter (not . (==) '\n')) p where
  p = T.pack <$> theChars <?> "account name"
  theChars = (:) <$> letter <*> rest
  rest = manyTill (anyChar <|> space) end <?> "rest of account name"
  end = (try $ string "  ") <|> string "\r" <|> string "\r\n" <|> (fmap (const "")  eof )
