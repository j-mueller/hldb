-- | Rendering of HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
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
  mapCallbacks,
  onClick
  )
import qualified Hledger.UI.FFI as FFI

type ElementID = Text
type ElementType = Text

data RenderingOptions = RenderingOptions{
  _remainingIDs :: [Text],
  _lastView :: Maybe (Elem () ElementID),
  _targetDivId :: Text
}

makeLenses ''RenderingOptions

renderingOptions :: Text -> RenderingOptions
renderingOptions = RenderingOptions ids Nothing where
  ids = fmap ((<>) "hldb-" . T.pack . show) [1..] -- infinite list of IDs

data RenderingAction e =
    DeleteElement{ _id :: ElementID }
  | NewElement{ _parentId :: ElementID, _elemDef :: e }
  | ChangeElement{ _elemDef :: e }
  deriving (Functor, Foldable, Traversable)

-- | The actual `diff` algorithm - compare the two `Element`s top-down to see
-- where they differ
-- At the moment it always creates everything from scratch :(
diff :: MonadState RenderingOptions m => ElementID -> Elem () ElementID -> Elem cb () -> m [RenderingAction (Elem cb ElementID)]
diff p old new = (:) <$> del <*> rest where
  rest = createNew new p
  del = return $ DeleteElement $ old^.elemID

-- | Create a tree of elements completely from scratch
createNew :: MonadState RenderingOptions m => Elem cb () -> ElementID -> m [RenderingAction (Elem cb ElementID)]
createNew elm i = do
  lastElem <- traverse (const nextId) elm
  lastView ?= mapCallbacks (const ()) lastElem
  return $ toNewElement i lastElem

-- | `RenderingAction`s for a single `Elem ElementID`
toNewElement :: ElementID -> Elem cb ElementID -> [RenderingAction (Elem cb ElementID)]
toNewElement i p = x:xs where
  x  = NewElement i p
  i' = p^.elemID
  xs = concat $ fmap (toNewElement i') $ p^.children

-- | Perform a single `RenderingAction`
renderAction :: RenderingAction (Elem (IO ()) ElementID) -> IO ()
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
  DeleteElement i -> FFI.js_deleteElementById $ textToJSString i
  _ -> undefined

-- | Perform a bunch of renderingActions
render :: [RenderingAction (Elem (IO ()) ElementID)] -> IO ()
render = fmap (const ()) . sequence . fmap renderAction

-- | Prepare a new version of the view (Elem f ()) to be rendered, using the
-- last known state from the RenderingOptions as a base line.
prepare :: RenderingOptions -> Elem ca () -> ([RenderingAction (Elem ca ElementID)], RenderingOptions)
prepare opts new = runState go opts where
  go       = maybe (createNew new target) (flip (diff target) new) original
  target   = opts^.targetDivId
  original = opts^.lastView

-- | Get a new id
nextId :: MonadState RenderingOptions m => m Text
nextId = remainingIDs %%= f where
  f = (,) <$> head <*> tail
