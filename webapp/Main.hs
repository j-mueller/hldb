{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens hiding (children, transform)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Monoid
import qualified Data.Text as T
import Prelude hiding (div)

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency
import Hledger.UI.Element
import Hledger.UI.Rendering
import Hledger.UI.Styles.Bootstrap

type View m a = a -> Elem (a -> m a) ()

theUI :: View Identity Int
theUI i = container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p "I am a paragraph, " & children .~ [strong "too"],
      if (i <= 5)
      then p ("I have been clicked " <> (T.pack $ show i) <> " times")
      else div & content .~ "DIV",
      btnDefault &
        content .~ "Submit" &
        callbacks . onClick ?~ return . succ]
    ]

renderUI :: RenderingOptions -> View m a -> (m a -> IO a) -> a -> IO ()
renderUI opts view interp state = render ioActions where
  newView = view state
  (actions, newOptions) = prepare opts newView
  updateUI = renderUI newOptions view interp -- updateUI :: a -> IO ()
  ioActions = fmap (mapCbs $ \f -> interp (f state) >>= updateUI) actions

main :: IO ()
main = do
  let options = renderingOptions "hldb"
  let interp = return . runIdentity
  renderUI options theUI interp 0
