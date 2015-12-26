{-# LANGUAGE TemplateHaskell #-}
module Hledger.Dashboard.Account where

import           Control.Lens
import           Data.AdditiveGroup
import           Data.Foldable
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Hledger.Dashboard.Currency (Currency)
import           Numeric.Interval

newtype Accounts = Accounts { _values :: M.Map String Currency }
  deriving (Eq, Ord)

makeLenses ''Accounts

instance Monoid Accounts where
  mempty = Accounts M.empty
  mappend l r = Accounts $ M.unionWith (^+^) (l^.values) (r^.values)

instance AdditiveGroup Accounts where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Accounts . fmap negateV . view values
