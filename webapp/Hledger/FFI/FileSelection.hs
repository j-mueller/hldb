{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GHCForeignImportPrim #-}
module Hledger.FFI.FileSelection(
  -- * Read file handler
  readFileInput,
  -- * Auxiliary types
  ClassName(..),
  doClick
  ) where

import           Control.Concurrent.MVar
import           Data.String (IsString(..))
import           Data.Text(Text)
import qualified Data.Text as T

import Data.JSString.Text
import GHCJS.Foreign.Callback
import GHCJS.Marshal 
import GHCJS.Prim
import GHCJS.Types (JSString, JSVal, JSRef)
import Prelude hiding (readFile)

-- | Read file from an `<input>` element with the given class name.
-- There should only be one element with that class name.
readFileInput :: ClassName -> IO (Either Text Text)
readFileInput cn = do
  v <- newEmptyMVar
  _ <- readFile' cn $ putMVar v
  takeMVar v

newtype SuccessHandler = SuccessHandler { getSuccessHandler :: JSVal  -> IO () }

newtype ErrorHandler = ErrorHandler { getErrorHandler :: JSVal -> IO () }

newtype ClassName = ClassName { getClassName :: Text }

instance IsString ClassName where
  fromString = ClassName . fromString

readFile' :: ClassName -> (Either Text Text -> IO ()) -> IO ()
readFile' cn cont = readFile_ cn (successHandler cont) (errorHandler cont)

successHandler :: (Either Text Text -> IO ()) -> SuccessHandler
successHandler cont = SuccessHandler $ cont . Right . textFromJSVal

errorHandler :: (Either Text Text -> IO ()) -> ErrorHandler
errorHandler cont = ErrorHandler $ cont . Left . textFromJSVal

readFile_ :: ClassName -> SuccessHandler -> ErrorHandler -> IO ()
readFile_ (ClassName cn) (SuccessHandler shdl) (ErrorHandler ehdl) = do
  let cn' = textToJSString cn 
  suc <- asyncCallback1 shdl
  er  <- asyncCallback1 ehdl
  js_hldb_readFile cn' suc er

doClick :: ClassName -> IO ()
doClick (ClassName cn) = do
  let cn' = textToJSString cn
  js_hldb_click cn'

foreign import javascript unsafe "hldb_readFile($1, $2, $3);"
  js_hldb_readFile :: JSString -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "hldb_click($1);"
  js_hldb_click :: JSString -> IO ()