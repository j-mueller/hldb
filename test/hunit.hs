{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative hiding (empty)
import           Control.Monad.State (StateT, evalState)
import           Data.Functor.Identity (Identity)
import           Data.Monoid ((<>))
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (fromGregorian)
import           Data.TreeMap (pathTo)
import           Data.Accounting.Account (account, accountNameP)
import           Data.Accounting.Currency (Currency, currency)
import           Data.Accounting.Transaction (
  Transaction(..),
  transactionP)
import           Data.Accounting.ParsingState (
  ParsingState,
  defaultParsingState)
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.Framework
import           Test.HUnit hiding (test)
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.Text
import           Text.Parsec.Prim


parseOnly ::  ParsecT Text () (StateT (ParsingState (Currency Text)) Identity) a -> String -> Either ParseError a
parseOnly p s = evalState (runParserT p () "" (T.pack s)) defaultParsingState

defSt :: ParsingState (Currency Text)
defSt = defaultParsingState

transaction1 :: String
transaction1 = "2015/12/12 beer\r\n    Expenses:Gifts  €10\r\n    Assets:Cash\r\n"

txn1Result :: Transaction
txn1Result = Transaction dt desc accts where
  dt    = fromGregorian 2015 12 12
  desc  = "beer"
  accts = gifts <> cash
  gifts = account $ pathTo ["Expenses", "Gifts"] $ currency (10 % 1) "€"
  cash  = account $ pathTo ["Assets", "Cash"] $ currency ((-10) % 1) "€"

transaction2 :: String
transaction2 = unlines [l1, l2, l3] where
  l1 = "2015/05/20 Bus ticket"
  l2 = "    Assets:Cash                                €-20.00"
  l3 = "    Expenses:Travel and Events:Work Travel     £14.41"

txn2Result :: Transaction
txn2Result = Transaction dt desc accts where
  dt = fromGregorian 2015 05 20
  desc = "Bus ticket"
  accts = cash <> exps
  cash = account $ pathTo ["Assets", "Cash"] $ currency (-20 % 1) "€"
  exps = account $ pathTo ["Expenses", "Travel and Events", "Work Travel"] $ currency (1441 % 100) "£"

transaction3 :: String
transaction3 = unlines [l1, l2, l3] where
  l1 = "2016/01/01 Exchange some money"
  l2 = "    CurrentAccount01      £22.00 @@ -20EUR"
  l3 = "    CurrentAccount01"

txn3Result :: Transaction
txn3Result = Transaction dt desc accts where
  dt = fromGregorian 2016 01 01
  desc = "Exchange some money"
  accts = c1 <> c2
  c1 = account $ pathTo ["CurrentAccount01"] $ currency (-20 % 1) "€"
  c2 = account $ pathTo ["CurrentAccount02"] $ currency (22 % 1) "£"

case_parse_transaction1 =
  parseOnly transactionP transaction1 @?= expected where
    expected = Right txn1Result

case_parse_transaction2 =
  parseOnly transactionP transaction2 @?= expected where
    expected = Right txn2Result

case_parse_transaction3 =
  parseOnly transactionP transaction3 @?= expected where
    expected = Right txn3Result

case_parse_account_names = result @?= expected where
  expected = Right ["Expenses", "Cash"]
  result = parseOnly accountNameP "Expenses:Cash"

main :: IO ()
main = defaultMain [$testGroupGenerator]
