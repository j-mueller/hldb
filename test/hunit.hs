{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Control.Applicative hiding (empty)
import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T
import           Hledger.Dashboard.Transaction
import           Hledger.Dashboard.ParsingState (
  ParsingState,
  defaultParsingState)
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.Framework
import           Test.HUnit hiding (test)
import           Text.Parsec.Text
import           Text.Parsec.Prim


parseOnly p s = evalState (runParserT p () "" (T.pack s)) defaultParsingState

transaction1 :: String
transaction1 = "2015/12/12 beer\r\n    Expenses:Gifts  €10\r\n    Assets:Cash\r\n"

case_parse_transaction =
  parseOnly transactionP transaction1 @?= expected where
    expected = undefined

main :: IO ()
main = defaultMain [$testGroupGenerator]
