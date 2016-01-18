{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens hiding (children, transform)
import Control.Monad.Fix
import Data.Monoid
import Prelude hiding (div)
import qualified Data.Text as T

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency
import Hledger.UI.Element
import Hledger.UI.Rendering
import Hledger.UI.Styles.Bootstrap

type View a = a -> Elem (a -> a) ()

theUI :: View Int
theUI i = container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p "I am a paragraph, " & children .~ [strong "too"],
      p ("I have been clicked " <> (T.pack $ show i) <> " times"),
      btnDefault &
        content .~ "Submit" &
        callbacks . onClick ?~ succ]
    ]

renderUI :: RenderingOptions -> View a -> a -> IO ()
renderUI opts view state = render ioActions where
  newView = view state
  (actions, newOptions) = prepare opts newView
  cb = renderUI newOptions view -- cb :: a -> IO ()
  ioActions = fmap (fmap (mapCallbacks $ \f -> cb $ f state)) actions

main :: IO ()
main = do
  let options = renderingOptions "hldb"
  renderUI options theUI 0
