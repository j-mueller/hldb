-- | Rendering of HTML elements
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
module Hledger.UI.Rendering where

import           Control.Lens
import           Data.JSString.Text (textToJSString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.Prim (JSRef)
import           GHCJS.Types (JSString)

import Hledger.UI.Element(
  Elem,
  elementType,
  content
  )

data RenderingOptions = RenderingOptions{
  _targetDivId :: Maybe Text
}

makeLenses ''RenderingOptions

defaultRenderingOptions :: RenderingOptions
defaultRenderingOptions = RenderingOptions Nothing

getParentElement :: RenderingOptions -> IO JSRef
getParentElement = maybe js_documentBody (js_getElementById . textToJSString) . view targetDivId

render :: RenderingOptions -> Elem -> IO ()
render opts e = do
  elm <- js_createElement  $ textToJSString $ e^.elementType
  t   <- js_createTextNode $ textToJSString $ e^.content
  _ <- js_appendChild elm t
  bd <- getParentElement opts
  js_appendChild bd elm

foreign import javascript unsafe "document.getElementById($1);"
  js_getElementById :: JSString -> IO JSRef

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSRef

foreign import javascript unsafe "document.body"
  js_documentBody :: IO JSRef

foreign import javascript unsafe "$1['appendChild']($2)"
  js_appendChild :: JSRef -> JSRef -> IO ()

foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO JSRef
