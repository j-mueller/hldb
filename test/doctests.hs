module Main where

import Test.DocTest

main :: IO ()
main = doctest [
  "-isrc",
  "src/Data/TreeMap.hs",
  "src/Data/Accounting/Account.hs",
  "src/Data/Accounting/Currency.hs",
  "src/Data/Accounting/Transaction.hs"]
