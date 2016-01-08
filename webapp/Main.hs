{-# LANGUAGE OverloadedStrings #-}
module Main where

import React.Flux

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency

main :: IO ()
main = reactRender "hldb" ledgerDashboard ()

ledgerDashboard :: ReactView ()
ledgerDashboard = defineView "header" $ \() ->
  header_ ["id" $= "header"] $ do
    h1_ "Ledger dashboard"
