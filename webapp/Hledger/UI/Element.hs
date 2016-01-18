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
import           Control.Lens hiding (children, transform)
import           Control.Monad.Cont
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Collection of callbacks of an element
data Callbacks cb = Callbacks{
  _onClick :: Maybe cb
  }
  deriving (Functor, Foldable, Traversable)

makeLenses ''Callbacks

emptyCb :: Callbacks cb
emptyCb = Callbacks Nothing

data Elem cb a = Elem{
  _elementType :: !Text,
  _attributes  :: !(Map Text Text),
  _content     :: !Text,
  _children    :: [Elem cb a],
  _elemID      :: !a,
  _callbacks   :: Callbacks cb
}
  deriving (Functor, Foldable, Traversable)

makeLenses ''Elem

-- | Transform the callbacks in an Elem
mapCallbacks :: (cb -> cc) -> Elem cb a -> Elem cc a
mapCallbacks f elm = elm{
  _children  = fmap (mapCallbacks f) $ elm^.children,
  _callbacks = fmap f $ elm^.callbacks
  }

-- | Create an element with the specified type
elm :: Text -> Elem cb ()
elm t = Elem t mempty mempty [] () emptyCb

elmWithContent :: Text -> Text -> Elem cb ()
elmWithContent t c = elm t & content .~ c

div :: Elem cb ()
div = elm "div"

h1 :: Text -> Elem cb ()
h1 = elmWithContent "h1"

p :: Text -> Elem cb ()
p = elmWithContent "p"

strong :: Text -> Elem cb ()
strong = elmWithContent "strong"

button :: Elem cb ()
button = elm "button"
