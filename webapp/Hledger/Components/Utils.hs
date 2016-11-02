{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.Utils where

import Control.Lens hiding (children)
import Data.Monoid
import Data.Text (Text)
import VirtualHom.Element

addClassName t = attributes . at "class" <>~ Just (t <> " ")