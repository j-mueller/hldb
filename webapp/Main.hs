{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Lens hiding (children)
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

theUI :: Elem ()
theUI = div_ & children .~ [
  h1_ "Hello, world",
  p_ "I am a paragraph, " & children .~ [strong_ "too"],
  p_ "Of course you are"]

main :: IO ()
main = do
  let options = RenderingOptions "hldb"
  e <- render options Nothing theUI
  putStrLn "Rendering complete"
  print e
