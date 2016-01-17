{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{- JS FFI functions for manipulating the DOM -}
module Hledger.UI.FFI where

import           GHCJS.Foreign.Callback
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

foreign import javascript unsafe "$1['setAttribute']($2, $3)"
  js_setAttribute :: JSRef -> JSString -> JSString -> IO ()

foreign import javascript unsafe "$1['onclick']=$2"
  js_setOnClick :: JSRef -> Callback (JSRef -> IO ()) -> IO ()

foreign import javascript unsafe "function(){e=document.getElementById($1);if(e!=null && e.parentElement!=null)e.parentElement.removeChild(e)}()"
  js_deleteElementById :: JSString -> IO ()
