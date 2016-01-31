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
import VirtualHom.Element
import VirtualHom.View
import VirtualHom.Html
import VirtualHom.Rendering

import Hledger.App

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

main :: IO ()
main = do
  let options = renderingOptions "hldb"
  let interp = return . runIdentity
  renderUI options theUI interp 0
