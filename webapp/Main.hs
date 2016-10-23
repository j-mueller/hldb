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
import VirtualHom.Components
import VirtualHom.Rendering(renderingOptions)

import Hledger.App

theUI :: Component Int
theUI = mempty

main :: IO ()
main = do
  let options = renderingOptions "hldb"
  renderComponent options theUI 0
