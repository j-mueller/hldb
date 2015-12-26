{-# LANGUAGE TemplateHaskell #-}
module Hledger.Dashboard.Currency where

import           Control.Lens hiding ((...), singular)
import           Data.AdditiveGroup
import           Data.List (intercalate)
import           Data.Monoid
import qualified Data.Map.Strict as M

newtype Currency = Currency { _values :: M.Map String Rational }
  deriving (Eq, Ord)

makeLenses ''Currency

instance Monoid Currency where
  mempty = Currency M.empty
  mappend l r = Currency $ M.unionWith (+) (l^.values) (r^.values)

instance AdditiveGroup Currency where
  zeroV = mempty
  l ^+^ r = l <> r
  negateV = Currency . fmap negate . view values

instance Show Currency where
  show c = intercalate " " $ fmap (\(k, v) -> show v ++ " " ++ k) $ M.assocs $ view values c
