{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{- JS FFI functions for manipulating the DOM -}
module Hledger.UI.FFI where

import           GHCJS.Prim (JSRef)
import           GHCJS.Types (JSString)

foreign import javascript unsafe "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSRef

foreign import javascript unsafe "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSRef

foreign import javascript unsafe "document.body"
  js_documentBody :: IO JSRef

foreign import javascript unsafe "$1['appendChild']($2)"
  js_appendChild :: JSRef -> JSRef -> IO ()

foreign import javascript unsafe "document.createElement($1)"
  js_createElement :: JSString -> IO JSRef

foreign import javascript unsafe "$1['id'] = $2"
  js_setId :: JSRef -> JSString -> IO ()
