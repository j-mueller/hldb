-- | HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Hledger.UI.Element where

import           Control.Lens
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

data Elem a = Elem{
  _elementType :: !Text,
  _attributes  :: !(M.Map Text Text),
  _content     :: !Text,
  _children    :: [Elem a],
  _elemID      :: !a
}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''Elem

div_ :: Elem ()
div_ = Elem "div" mempty "" [] ()

h1_ :: Text -> Elem ()
h1_ t = Elem "h1" mempty t [] ()

p_ :: Text -> Elem ()
p_ t = Elem "p" mempty t [] ()

strong_ :: Text -> Elem ()
strong_ t = Elem "strong" mempty t [] ()
