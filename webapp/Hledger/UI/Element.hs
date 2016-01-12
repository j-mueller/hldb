-- | HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.UI.Element where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

data Elem = Elem{
  _elementType :: !Text,
  _attributes  :: !(M.Map Text Text),
  _content     :: !Text,
  _childen     :: [Elem]
}

makeLenses ''Elem

h1_ :: Text -> Elem
h1_ t = Elem "h1" mempty t []

h2_ :: Text -> Elem
h2_ t = Elem "h2" mempty t []
