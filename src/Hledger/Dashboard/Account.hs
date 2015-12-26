{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Hledger.Dashboard.Account where

import           Control.Lens
import           Data.AdditiveGroup
import           Data.Foldable
import           Data.Monoid
import           Hledger.Dashboard.Currency (Currency)

data AccountDetails = AccountDetails {
  _accountName :: Maybe String,
  _accountBalance :: Currency -- balance excluding sub-accounts
}

makeLenses ''AccountDetails

data Tree a = Tree {
  _node :: a,
  _children :: [Tree a]
}
  deriving (Functor, Foldable, Traversable)

makeLenses ''Tree

-- | Merge two `Account`s
merge :: Tree AccountDetails -> Tree AccountDetails -> Tree AccountDetails
merge = undefined

instance Monoid (Tree AccountDetails) where
  mempty  = Tree (AccountDetails mempty mempty) []
  mappend = merge

instance AdditiveGroup (Tree AccountDetails) where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = fmap (over accountBalance negateV)

type Account = Tree AccountDetails
