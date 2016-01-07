module Main where

import Test.DocTest

main :: IO ()
main = doctest [
  "-isrc",
  "src/Data/TreeMap.hs",
  "src/Hledger/Dashboard/Account.hs",
  "src/Hledger/Dashboard/Currency.hs",
  "src/Hledger/Dashboard/Transaction.hs"]
