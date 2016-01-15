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
import           Control.Monad.Cont
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Collection of callbacks of an element
data Callbacks m = Callbacks{
  _onClick :: Maybe (m ())
  }

makeLenses ''Callbacks

emptyCb = Callbacks Nothing

data Elem m a = Elem{
  _elementType :: !Text,
  _attributes  :: !(Map Text Text),
  _content     :: !Text,
  _children    :: [Elem m a],
  _elemID      :: !a,
  _callbacks   :: Callbacks m
}
  deriving (Functor, Foldable, Traversable)

makeLenses ''Elem

-- | Create an element with the specified type
elm :: Text -> Elem m ()
elm t = Elem t mempty mempty [] () emptyCb

elmWithContent :: Text -> Text -> Elem m ()
elmWithContent t c = elm t & content .~ c

div :: Elem m ()
div = elm "div"

h1 :: Text -> Elem m ()
h1 = elmWithContent "h1"

p :: Text -> Elem m ()
p = elmWithContent "p"

strong :: Text -> Elem m ()
strong = elmWithContent "strong"

button :: Elem m ()
button = elm "button"
