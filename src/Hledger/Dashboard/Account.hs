{-# LANGUAGE TemplateHaskell #-}
module Hledger.Dashboard.Account(
  Accounts(..),
  accounts,
  empty,
  merge
) where

import           Control.Lens hiding (children)
import           Data.AdditiveGroup
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.TreeMap (TreeMap(..))
import           Hledger.Dashboard.Currency (Currency)

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Test.QuickCheck hiding (scale)
-- >>> import Hledger.Dashboard.Currency (Currency(..))
-- >>> instance Arbitrary Currency where arbitrary = Currency <$> fmap M.fromList arbitrary
-- >>> instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where arbitrary = TreeMap <$> arbitrary <*> (fmap M.fromList $ fmap (\l -> if length l > 2 then [] else l) arbitrary)
-- >>> instance Arbitrary Accounts where arbitrary = Accounts <$> fmap M.fromList arbitrary
-- >>> :set -XScopedTypeVariables

newtype Accounts = Accounts { _accounts :: M.Map String (TreeMap String Currency) }
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
