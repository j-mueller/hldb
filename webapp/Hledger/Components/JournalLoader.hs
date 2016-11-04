{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.JournalLoader where
  
import           Control.Lens hiding (children)
import           Control.Monad.IO.Class
import           Data.Accounting.Journal (Journal)
import           Data.Text (Text)
import           Hledger.Components.Utils (addClassName)
import           Hledger.FFI.FileSelection (doClick, readFileInput)
import           VirtualHom.Element 
import qualified VirtualHom.Html as H
import           VirtualHom.Internal.Handler  
import           VirtualHom.Components

journalLoader :: Component (Maybe Journal)
journalLoader = component' $ \_ -> [
  H.div & addClassName "data-sources" & children .~ [
  H.div & addClassName "title" & content .~ "Select data source",
  H.div 
    & addClassName "data-sources-container"
    & children .~ [
      makeFS "Ledger file" "Load a ledger file from disk",
      makeDS "Random data" "Use a randomly generated data set"
        & callbacks . click ?~ const selectRandomData
  ]]] where
    makeDS ttl desc = H.div & addClassName "item-card" & children .~ [
      H.div & addClassName "inner-container" & children .~ [
          H.div & addClassName "text" & children .~ [
            H.div & addClassName "title" & content .~ ttl,
            H.div & addClassName "description" & content .~ desc
          ]
      ]]
    makeFS ttl desc = H.div & addClassName "item-card" & children .~ [
      H.input
        & attributes . at "class" ?~ "ledger-file-input" 
        & attributes . at "type" ?~ "file" 
        & attributes . at "style" ?~ "display: none;"
        & callbacks . change ?~ const selectLedgerFile,
      H.div 
        & addClassName "inner-container"
        & callbacks . click ?~ const clickFileSelector 
        & children .~ [
          H.div & addClassName "text" & children .~ [
            H.div & addClassName "title" & content .~ ttl,
            H.div & addClassName "description" & content .~ desc
          ]
      ]]

clickFileSelector :: Handler (Maybe Journal -> Maybe Journal) ()
clickFileSelector = liftIO $ doClick "ledger-file-input"

selectLedgerFile :: Handler (Maybe Journal -> Maybe Journal) ()
selectLedgerFile = do
  result <- liftIO $ readFileInput "ledger-file-input"
  liftIO $ putStrLn $ show result
-- TODO: https://developer.mozilla.org/en-US/docs/Using_files_from_web_applications#Using_hidden_file_input_elements_using_the_click()_method

selectRandomData :: Handler (Maybe Journal -> Maybe Journal) ()
selectRandomData = liftIO $ putStrLn "Random data"