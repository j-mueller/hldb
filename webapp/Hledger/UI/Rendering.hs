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
import           GHCJS.Foreign.Callback (asyncCallback, asyncCallback1)

import Hledger.UI.Element(
  Callbacks,
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
type VirtualElem = Elem () ElementID -- an element whose callbacks are of type ()

data RenderingOptions = RenderingOptions{
  _remainingIDs :: [Text],
  _lastView :: Maybe VirtualElem,
  _targetDivId :: Text
}

makeLenses ''RenderingOptions

renderingOptions :: Text -> RenderingOptions
renderingOptions = RenderingOptions ids Nothing where
  ids = fmap ((<>) "hldb-" . T.pack . show) [1..] -- infinite list of IDs

data RenderingAction c e =
    DeleteElement{ _id :: ElementID }
  | NewElement{ _parentId :: ElementID, _elemDef :: e }
  | SetTextContent{ _id :: ElementID, _text :: Text }
  | RemoveAttribute{ _id :: ElementID, _attribute :: Text }
  | SetAttribute{ _id :: ElementID, _attribute :: Text, _attrValue :: Text }
  | SetCallback{ _id :: ElementID, _callbackName :: Text, _callback :: c }
  | RemoveCallback{ _id :: ElementID, _callbackName :: Text }
  | NoAction
  deriving (Functor, Foldable, Traversable)

mapCbs :: (c -> d) -> RenderingAction c (Elem c a) -> RenderingAction d (Elem d a)
mapCbs f = mapCb . fmap (mapCallbacks f) where
  mapCb (DeleteElement i)     = DeleteElement i
  mapCb (NewElement i d)      = NewElement i d
  mapCb (SetTextContent i t)  = SetTextContent i t
  mapCb (RemoveAttribute i a) = RemoveAttribute i a
  mapCb (SetAttribute i a v)  = SetAttribute i a v
  mapCb (SetCallback i n c)   = SetCallback i n $ f c
  mapCb (RemoveCallback i n)  = RemoveCallback i n
  mapCb NoAction = NoAction

-- | The actual `diff` algorithm - compare the two `Element`s top-down to see
-- where they differ
diff :: MonadState RenderingOptions m =>
  ElementID -> -- ^ ID of parent element (for inserting new elements)
  Elem () ElementID -> -- ^ previous elem (diff baseline)
  Elem cb () -> -- ^ new elem
  m [RenderingAction cb (Elem cb ElementID)]
diff p old new = do
  newWithIds <- traverse (const nextId) new
  let (result, substitutions) = diff' p old newWithIds
  let substitute t = maybe t id $ M.lookup t substitutions
  let lastView' = mapCallbacks (const ()) $ fmap substitute newWithIds
  lastView ?= lastView'
  return result

-- | Actual implementation of diff.
diff' ::
  ElementID ->
  Elem () ElementID ->
  Elem cb ElementID ->
  ([RenderingAction cb (Elem cb ElementID)], Map ElementID ElementID)
diff' p old new
  | old^.elementType == new^.elementType = diffSameType old new
  | otherwise = (del:rest, M.empty) where
    del  = DeleteElement $ old^.elemID
    rest = createNew p new

-- | Generate actions for changing an existing element into a new one, assuming
-- both have the same type.
diffSameType ::
  Elem () ElementID -> -- ^ Old
  Elem cb ElementID -> -- ^ New
  ([RenderingAction cb (Elem cb ElementID)], Map ElementID ElementID) -- ^ Actions to get from old to new
diffSameType old new = (contAct <> attrAct <> cbAct <> childAct, subst <> childSubst) where
  -- 1. the ID of old element should be kept
  targetId = old^.elemID
  subst = M.fromList [(new^.elemID, targetId)]
  -- 2. check if the content needs to be updated
  contAct =  if (new^.content == old^.content)
                          then [NoAction]
                          else [SetTextContent targetId $ new^.content]
  -- 3. Update the element's attributes
  attrAct = changeAttributes (old^.attributes) (new^.attributes) targetId
  -- 4. Update the element's callbacks
  cbAct = changeCallbacks (old^.callbacks) (new^.callbacks) targetId
  -- 5. Update the element's children
  (childAct, childSubst) = diffChildren targetId (old^.children) (new^.children)

diffChildren ::
  ElementID ->
  [Elem () ElementID] ->
  [Elem cb ElementID] ->
  ([RenderingAction cb (Elem cb ElementID)], Map ElementID ElementID)
diffChildren pId [] xs = (concat $ fmap (createNew pId) xs, M.empty)
diffChildren _   ys [] = (fmap (DeleteElement . view elemID) ys, M.empty)
diffChildren pId (x:xs) (y:ys) = (firstDiff <> restDiffs, firstSubst <> restSubst) where
  (firstDiff, firstSubst) = diff' pId x y
  (restDiffs, restSubst ) = diffChildren pId xs ys

changeCallbacks :: Callbacks ca -> Callbacks cb -> ElementID -> [RenderingAction cb a]
changeCallbacks old new i = [onclick] where
  onclick = case (old^.onClick, new^.onClick) of
    (Nothing, Nothing) -> NoAction
    (Just _,  Nothing) -> RemoveCallback i "onclick"
    (_,       Just a)  -> SetCallback i "onclick" a

-- | Takes the map with old attributes and the map with new attributes and
-- generates actions that will transform an element with the first set of
-- attributes to one with the second set of attributes
changeAttributes :: Map Text Text -> Map Text Text -> ElementID -> [RenderingAction cb a]
changeAttributes old new i = actions where
  actions = fmap snd $ M.toList $ inner old new
  inner = M.mergeWithKey join mapOld mapNew
  join k a b
    | a == b    = Nothing
    | otherwise = Just $ SetAttribute i k b
  mapOld = M.mapWithKey $ \k _ -> RemoveAttribute i k
  mapNew = M.mapWithKey $ \k v -> SetAttribute i k v

-- | `RenderingAction`s for a single `Elem ElementID`
createNew :: ElementID -> Elem cb ElementID -> [RenderingAction cb (Elem cb ElementID)]
createNew i p = x:xs where
  x  = NewElement i p
  i' = p^.elemID
  xs = concat $ fmap (createNew i') $ p^.children

-- | Perform a single `RenderingAction`
renderAction :: RenderingAction (IO ()) (Elem (IO ()) ElementID) -> IO ()
renderAction a = case a of
  NewElement p def -> do
    _ <- putStrLn ("Creating new element with parent: " <> show p)
    elm <- FFI.js_createElement $ textToJSString $ def^.elementType
    t <- FFI.js_createTextNode $ textToJSString $ def^.content
    _ <- FFI.js_appendChild elm t
    _ <- sequence $ fmap (uncurry $ FFI.js_setAttribute elm) $ fmap ((,) <$> textToJSString . fst <*> textToJSString . snd) $ M.toList $ def^.attributes
    b <- FFI.js_getElementById $ textToJSString p
    _ <- FFI.js_setId elm $ textToJSString $ view elemID def
    _ <- maybe (return ()) (\c -> asyncCallback1 (const c) >>= FFI.js_setOnClick elm) $ def^.callbacks.onClick
    FFI.js_appendChild b elm
  DeleteElement i -> do
    _ <- putStrLn $ "Deleting element: " <> show i
    FFI.js_deleteElementById $ textToJSString i
  RemoveAttribute i a -> FFI.js_removeAttributeById (textToJSString i) $ textToJSString a
  NoAction -> return ()
  RemoveCallback i n -> do
    _ <- putStrLn $ "Removing callback " ++ (show n) ++ " from " ++ (show i)
    FFI.js_RemoveCallbackById (textToJSString i) $ textToJSString n
  SetCallback i n c -> do
    _ <- putStrLn $ "Changing callback " ++ (show n) ++ " of " ++ (show i)
    (asyncCallback c) >>= FFI.js_setCallbackById (textToJSString i) (textToJSString n)
  SetTextContent i t -> do
    _ <- putStrLn $ "Set text content of " ++ (show i) ++ " to " ++ (show t)
    FFI.js_setTextContent (textToJSString i) (textToJSString t)
  SetAttribute i a v -> FFI.js_setAttributeById (textToJSString i) (textToJSString a) (textToJSString v)

-- | Perform a bunch of renderingActions
render :: [RenderingAction (IO ()) (Elem (IO ()) ElementID)] -> IO ()
render = fmap (const ()) . sequence . fmap renderAction . filter (not . isNop) where
  isNop NoAction = True
  isNop _        = False

-- | Prepare a new version of the view (Elem ca ()) to be rendered, using the
-- last known state from the RenderingOptions as a base line.
prepare :: RenderingOptions ->  Elem ca () -> ([RenderingAction ca (Elem ca ElementID)], RenderingOptions)
prepare opts new = runState go opts where
  go       = maybe makeNew (flip (diff target) new) old
  target   = opts^.targetDivId
  old      = opts^.lastView
  makeNew  = do
    newWithIds <- traverse (const nextId) new
    let lastView' = mapCallbacks (const ()) newWithIds
    lastView ?= lastView'
    let result = createNew target newWithIds
    return result

-- | Get a new id
nextId :: MonadState RenderingOptions m => m Text
nextId = remainingIDs %%= f where
  f = (,) <$> head <*> tail
