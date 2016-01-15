{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Lens hiding (children, transform)
import Control.Monad.Cont
import Prelude hiding (div)

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency
import Hledger.UI.Element
import Hledger.UI.Rendering
import Hledger.UI.Styles.Bootstrap

newtype DashboardGUI a = DashboardGUI { _gui :: ContT () IO a }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadCont,
    MonadIO
  )

theUI :: Elem ()
theUI = container & children .~ [
  row & children .~ [
    h1 "Hello, world",
    p "I am a paragraph, " & children .~ [strong "too"],
    p "Of course you are",
    btnDefault &
      content .~ "Submit" &
      callbacks . onClick ?~ putStrLn "Click"]
  ]

main :: IO ()
main = do
  let options = RenderingOptions "hldb"
  e <- render options Nothing theUI
  putStrLn "Rendering complete"
