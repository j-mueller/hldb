-- | Rendering of HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hledger.UI.Rendering where

import           Control.Applicative
import           Control.Lens hiding (children)
import           Control.Monad.State
import           Data.JSString.Text (textToJSString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import Hledger.UI.Element(
  Elem,
  children,
  content,
  elementType,
  elemID
  )
import qualified Hledger.UI.FFI as FFI

data RenderingOptions = RenderingOptions{
  _targetDivId :: Text
}

makeLenses ''RenderingOptions

type ElementID = Text
type ElementType = Text

data RenderingAction =
    DeleteElement{ _id :: ElementID }
  | NewElement{ _parentId :: ElementID, _elemDef :: Elem ElementID }
  | ChangeElement{ _elemDef :: Elem ElementID }
  deriving (Eq, Ord, Show)

-- | The actual `diff` algorithm - compare the two `Element`s top-down to see
-- where they differ
diff :: MonadState [Text] m => Elem ElementID -> Elem () -> m ([RenderingAction], Elem ElementID)
diff old new = createNew new $ old^.elemID

-- | Create a tree of elements completely from scratch
createNew ::  MonadState [Text] m => Elem () -> ElementID -> m ([RenderingAction], Elem ElementID)
createNew elm i = fmap tp $ traverse (const nextId) elm where
  tp = (,) <$> toNewElement i <*> id

-- | `RenderingAction`s for a single `Elem ElementID`
toNewElement :: ElementID -> Elem ElementID -> [RenderingAction]
toNewElement i p = x:xs where
  x  = NewElement i p
  i' = p^.elemID
  xs = concat $ fmap (toNewElement i') $ p^.children

-- | Perform a single `RenderingAction`
renderAction :: RenderingAction -> IO ()
renderAction a = case a of
  NewElement p def -> do
    elm <- FFI.js_createElement $ textToJSString $ def^.elementType
    t <- FFI.js_createTextNode $ textToJSString $ def^.content
    _ <- FFI.js_appendChild elm t
    b <- FFI.js_getElementById $ textToJSString p
    _ <- FFI.js_setId elm $ textToJSString $ view elemID def
    FFI.js_appendChild b elm
    putStrLn $ "Rendered " ++ (show def)
  _ -> undefined

render :: RenderingOptions -> Maybe (Elem ElementID) -> Elem () -> IO (Elem ElementID)
render opts original new = evalStateT (renderer go) ids where
  go = do
    let i = opts^.targetDivId
    (actions, newElem) <- maybe (createNew new i) (flip diff new) original
    _ <- sequence $ fmap (liftIO . renderAction) actions
    return newElem

-- | A list of ids that can be used by hldb elements
ids :: [Text]
ids = fmap ((<>) "hldb" . T.pack . show) [1..]

-- | Get a new id
nextId :: MonadState [Text] m => m Text
nextId = state f where
  f = (,) <$> head <*> tail

newtype Renderer a = Renderer{ renderer :: StateT [Text] IO a }
  deriving (Functor, Applicative, Monad, MonadState [Text], MonadIO)
