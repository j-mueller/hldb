{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Cont

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency
import Hledger.UI.Element
import Hledger.UI.Rendering

import Styles

newtype DashboardGUI a = DashboardGUI { _gui :: ContT () IO a }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadCont,
    MonadIO
  )

e1 = h1_ "Header 1"

main :: IO ()
main = do
  render defaultRenderingOptions e1
  putStrLn "Rendering complete"
