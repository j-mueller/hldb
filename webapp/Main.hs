{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Lens hiding (children, transform)
import Control.Monad.Fix
import Prelude hiding (div)
import qualified Data.Text as T

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency
import Hledger.UI.Element
import Hledger.UI.Rendering
import Hledger.UI.Styles.Bootstrap

-- UI of state s should be MonadWriter (Elem ()) m => s -> m s

theUI :: Int -> Elem IO ()
theUI i = container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p "I am a paragraph, " & children .~ [strong "too"],
      p "Of course you are",
      btnDefault &
        content .~ "Submit" &
        callbacks . onClick ?~ (putStrLn "click")]
    ]

main :: IO ()
main = do
  let options = RenderingOptions "hldb"
  e <- render options Nothing $ theUI 10
  putStrLn "Rendering complete"
