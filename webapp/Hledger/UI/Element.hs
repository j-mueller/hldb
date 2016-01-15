-- | HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Hledger.UI.Element where

import           Control.Applicative
import           Control.Lens hiding (transform)
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Collection of callbacks of an element
data Callbacks = Callbacks{
  _onClick :: Maybe (IO ())
  }

makeLenses ''Callbacks

emptyCb = Callbacks Nothing

data Elem a = Elem{
  _elementType :: !Text,
  _attributes  :: !(Map Text Text),
  _content     :: !Text,
  _children    :: [Elem a],
  _elemID      :: !a,
  _callbacks   :: Callbacks
}
  deriving (Functor, Foldable, Traversable)

makeLenses ''Elem

-- | Create an element with the specified type
elm :: Text -> Elem ()
elm t = Elem t mempty mempty [] () emptyCb

elmWithContent :: Text -> Text -> Elem ()
elmWithContent t c = elm t & content .~ c

div :: Elem ()
div = elm "div"

h1 :: Text -> Elem ()
h1 = elmWithContent "h1"

p :: Text -> Elem ()
p = elmWithContent "p"

strong :: Text -> Elem ()
strong = elmWithContent "strong"

button :: Elem ()
button = elm "button"
