module Main where

import Test.DocTest

main :: IO ()
main = doctest [
  "-isrc",
  "src/Hledger/Dashboard/Currency.hs",
  "src/Hledger/Dashboard/Account.hs"]
