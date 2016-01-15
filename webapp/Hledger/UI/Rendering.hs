-- | Rendering of HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           GHCJS.Foreign.Callback (asyncCallback1)

import Hledger.UI.Element(
  Elem,
  attributes,
  callbacks,
  children,
  content,
  elementType,
  elemID,
  onClick
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

-- | The actual `diff` algorithm - compare the two `Element`s top-down to see
-- where they differ
-- At the moment it always creates everything from scratch :(
diff :: MonadState [Text] m => ElementID -> Elem ElementID -> Elem () -> m ([RenderingAction], Elem ElementID)
diff p old new = createNew new p

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
    _ <- sequence $ fmap (uncurry $ FFI.js_setAttribute elm) $ fmap ((,) <$> textToJSString . fst <*> textToJSString . snd) $ M.toList $ def^.attributes
    b <- FFI.js_getElementById $ textToJSString p
    _ <- FFI.js_setId elm $ textToJSString $ view elemID def
    _ <- maybe (return ()) (\c -> asyncCallback1 (const c) >>= FFI.js_setOnClick elm) $ def^.callbacks.onClick
    FFI.js_appendChild b elm
  _ -> undefined

render :: RenderingOptions -> Maybe (Elem ElementID) -> Elem () -> IO (Elem ElementID)
render opts original new = evalStateT (renderer go) ids where
  go = do
    let i = opts^.targetDivId
    (actions, newElem) <- maybe (createNew new i) (flip (diff i) new) original
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
