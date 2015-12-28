{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Hledger.Dashboard.Account(
  Accounts(..),
  accounts,
  empty,
  merge,
  -- * Utility type
  TreeMap(..),
  node,
  children
) where

import           Control.Lens hiding (children)
import           Data.AdditiveGroup
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Hledger.Dashboard.Currency (Currency)

-- $setup
-- >>> import Control.Applicative hiding (empty)
-- >>> import Test.QuickCheck hiding (scale)
-- >>> import Hledger.Dashboard.Currency (Currency(..))
-- >>> instance Arbitrary Currency where arbitrary = Currency <$> fmap M.fromList arbitrary
-- >>> instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where arbitrary = TreeMap <$> arbitrary <*> (fmap M.fromList $ fmap (\l -> if length l > 2 then [] else l) arbitrary)
-- >>> instance Arbitrary Accounts where arbitrary = Accounts <$> fmap M.fromList arbitrary
-- >>> :set -XScopedTypeVariables

data TreeMap k v = TreeMap {
  _node :: !v,
  _children :: !(M.Map k (TreeMap k v))
}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''TreeMap

instance Monoid (TreeMap String Currency) where
  mempty  = TreeMap mempty M.empty
  mappend l r = TreeMap n c where
    n = (l^.node) <> (r^.node)
    c = M.unionWith (<>) (l^.children) (r^.children)

instance AdditiveGroup (TreeMap String Currency) where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = fmap negateV

newtype Accounts = Accounts { _accounts :: M.Map String (TreeMap String Currency) }
  deriving (Eq, Ord, Show)

makeLenses ''Accounts

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

instance Monoid Accounts where
  mempty = empty
  mappend = merge
