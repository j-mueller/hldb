{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative hiding (empty)
import           Control.Lens ((&), (.~), _1, _2, _3, over, view)
import           Control.Monad.State (StateT, evalState)
import           Data.AdditiveGroup
import           Data.Accounting.Account (Accounts(..))
import           Data.Accounting.Currency (Currency(..))
import           Data.Accounting.Journal (Journal(..), insert, singleton, splitByDay, transactions)
import           Data.Accounting.Transaction (Transaction(..), date)
import           Data.Functor.Identity (Identity)
import           Data.Foldable (foldl', toList)
import           Data.List (sortOn)
import qualified Data.Map as M
import           Data.Semigroup (mempty, (<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day, fromGregorian)
import           Data.TreeMap (pathTo)
import           Data.TreeMap (TreeMap(..))
import           Data.VectorSpace
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.Framework
import           Test.QuickCheck hiding (scale)

instance Arbitrary Text where 
  arbitrary = T.pack <$> arbitrary

instance Arbitrary (Currency Text) where 
  arbitrary = Currency <$> fmap M.fromList arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TreeMap k v) where 
  arbitrary = TreeMap <$> arbitrary <*> (fmap M.fromList $ fmap (\l -> if length l > 2 then [] else l) arbitrary)

instance Arbitrary Accounts where 
  arbitrary = Accounts <$> arbitrary

instance Arbitrary Day where
  arbitrary = fromGregorian <$> elements [1900..2100] <*> elements [1..12] <*> elements [1..31]

instance Arbitrary Transaction where
  arbitrary = Transaction <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = defaultMain [$testGroupGenerator]

prop_merge_unit :: Accounts -> Bool
prop_merge_unit ac = mempty <> ac == ac

prop_merge_associative :: Accounts -> Accounts -> Accounts -> Bool
prop_merge_associative a b c = (a <> b) <> c == a <> (b <> c)

prop_merge_commutative :: Accounts -> Accounts -> Bool
prop_merge_commutative l r = l <> r == r <> l

prop_currency_scale_zero :: Currency Text -> Bool
prop_currency_scale_zero c = 0 *^ c == mempty

prop_currency_scale_one :: Currency Text -> Bool
prop_currency_scale_one c = 1 *^ c == c

prop_currency_plus_unit :: Currency Text -> Bool
prop_currency_plus_unit c = c <> mempty == c

prop_currency_plus_associative :: Currency Text -> Currency Text -> Currency Text -> Bool
prop_currency_plus_associative a b c = (a <> b) <> c == a <> (b <> c)

prop_currency_plus_commutative :: Currency Text -> Currency Text -> Bool
prop_currency_plus_commutative l r = l <> r == r <> l

prop_currency_invert_double :: Currency Text -> Bool
prop_currency_invert_double c = (negateV $ negateV c) == c

prop_currency_invert_cancels :: Currency Text -> Bool
prop_currency_invert_cancels c = c ^+^ (negateV c) == zeroV

prop_journal_insert_all :: [Transaction] -> Bool
prop_journal_insert_all txns = (length $ transactions (foldMap singleton txns)) == (length txns)  

prop_journal_ordered :: [Transaction] -> Bool
prop_journal_ordered txns = expected == actual where
  actual = transactions $ foldMap singleton txns
  expected = sortOn (view date) $ reverse txns -- transactions on the same day are reversed

prop_journal_split :: Transaction -> Day -> Bool
prop_journal_split t d = expected == actual where
  t1' = t & date .~ pred d
  t2' = t & date .~ d
  t3' = t & date .~ succ d
  (Journal theJournal) = foldl' (flip insert) mempty [t2', t1', t3']
  expected = ([t1'], [t2'], [t3']) 
  (before, duringAndAfter) = splitByDay d theJournal
  (during, after) = splitByDay (succ d) duringAndAfter
  actual = (toList before, toList during, toList after)