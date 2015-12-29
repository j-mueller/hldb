{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.TreeMap(
  TreeMap(..),
  node,
  children,
  -- * Constructors
  empty,
  leaf,
  -- * Combinators
  addChild,
  merge
) where

import           Control.Lens hiding (children)
import           Data.AdditiveGroup
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Monoid

-- | A tree whose children are indexed by a key rather than kept in a list
data TreeMap k v = TreeMap {
  _node :: !v,
  _children :: !(M.Map k (TreeMap k v))
}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''TreeMap

instance (Monoid v, Ord k) => Monoid (TreeMap k v) where
  mempty  = empty
  mappend = merge

instance (AdditiveGroup v, Monoid v, Ord k) => AdditiveGroup (TreeMap k v) where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = fmap negateV

-- | Empty `TreeMap`, if `k` is a `Monoid`
empty :: Monoid v => TreeMap k v
empty = TreeMap mempty M.empty

-- | Combine two `TreeMap`s using the `Monoid` instances of their values
merge :: (Monoid v, Ord k) => TreeMap k v -> TreeMap k v -> TreeMap k v
merge l r = TreeMap n c where
  n = (l^.node) <> (r^.node)
  c = M.unionWith (<>) (l^.children) (r^.children)

-- | A `TreeMap` without any children
leaf :: a -> TreeMap k a
leaf a = TreeMap a M.empty

-- | Add a child to a `TreeMap`
addChild :: (Monoid v, Ord k) => TreeMap k v -> (k, TreeMap k v) -> TreeMap k v
addChild parent (k, v) = parent & children %~ M.insertWith mappend k v
