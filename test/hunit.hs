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
import           Hledger.Dashboard.Account (account, accountNameP)
import           Hledger.Dashboard.Currency (Currency, currency)
import           Hledger.Dashboard.Transaction (
  Transaction(..),
  transactionP)
import           Hledger.Dashboard.ParsingState (
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

case_parse_transaction =
  parseOnly transactionP transaction1 @?= expected where
    expected = Right txn1Result

case_parse_account_names = result @?= expected where
  expected = Right ["Expenses", "Cash"]
  result = parseOnly accountNameP "Expenses:Cash"

main :: IO ()
main = defaultMain [$testGroupGenerator]
