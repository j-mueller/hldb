{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Dashboard.Account(
  Accounts(..),
  accounts,
  empty,
  account,
  -- * Combinators
  merge,
  -- * Parser
  accountP
) where

import           Control.Applicative hiding (empty)
import           Control.Lens hiding (children)
import           Data.AdditiveGroup
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TreeMap (TreeMap(..))
import           Hledger.Dashboard.Currency (Currency)

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Data.Text (Text)
-- >>> import qualified Data.Text as T
-- >>> import Test.QuickCheck hiding (scale)
-- >>> import Hledger.Dashboard.Currency (Currency(..))
-- >>> :set -XScopedTypeVariables
-- >>> :set -XFlexibleInstances
-- >>> instance Arbitrary Text where arbitrary = T.pack <$> arbitrary
-- >>> instance Arbitrary (Currency Text) where arbitrary = Currency <$> fmap M.fromList arbitrary
-- >>> instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where arbitrary = TreeMap <$> arbitrary <*> (fmap M.fromList $ fmap (\l -> if length l > 2 then [] else l) arbitrary)
-- >>> instance Arbitrary Accounts where arbitrary = Accounts <$> fmap M.fromList arbitrary

-- | An account is a `TreeMap String Currency` and `Accounts` is a top-level
--   account.
newtype Accounts = Accounts { _accounts :: M.Map Text (TreeMap Text (Currency Text)) }
  deriving (Eq, Ord, Show)

makeLenses ''Accounts

instance Monoid Accounts where
  mempty = empty
  mappend = merge

instance AdditiveGroup Accounts where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Accounts . M.map negateV . view accounts

-- | An empty set of `Accounts`
empty :: Accounts
empty = Accounts M.empty

-- | Merge two `Accounts`'
--
-- prop> \(a :: Accounts) -> merge a empty == a
-- prop> \(a :: Accounts) -> merge empty a == a
-- prop> \((a, b, c) :: (Accounts, Accounts, Accounts)) -> (a `merge` b) `merge` c == a `merge` (b `merge` c)
-- prop> \((l, r) :: (Accounts, Accounts)) -> l `merge` r == r `merge` l
merge :: Accounts -> Accounts -> Accounts
merge l r = Accounts $ M.unionWith (<>) (view accounts l) (view accounts r)

-- | Create an `Accounts` object with a single top-level account
account :: Text -> TreeMap Text (Currency Text) -> Accounts
account n = Accounts . M.singleton n

-- | Parse an `Accounts` value. Each node in the result will have at most one
-- child.
accountP :: Parser Accounts
accountP = undefined -- TODO:

-- | Parse the name of a single account (not in hierarchy).
accountNameP :: Parser Text
accountNameP = T.pack <$> theChars where
  theChars = (:) <$> letter <*> rest
  rest = manyTill anyChar end
  end = string "  " <|> string "\r" <|> string "\r\n" <|> (fmap (const "")  endOfInput)
