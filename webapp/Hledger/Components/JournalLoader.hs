{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.JournalLoader where
  
import           Control.Lens hiding (children)
import           Control.Monad.IO.Class
import           Data.Accounting.Journal (Journal, totalBalance)
import           Data.Accounting.Parser (parseJournal)
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
      makeDS "Random data" "(not implemented yet)"
        & callbacks . click ?~ const selectRandomData
  ]]] where
    makeDS ttl desc = H.div & addClassName "item-card unavailable" & children .~ [
      H.div & addClassName "inner-container" & children .~ [
          H.div & addClassName "text" & children .~ [
            H.div & addClassName "title unavailable" & content .~ ttl,
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
  either (const $ liftIO $ putStrLn "ERROR") (update . const . Just) $ (result >>= parseJournal)

selectRandomData :: Handler (Maybe Journal -> Maybe Journal) ()
selectRandomData = liftIO $ putStrLn "Random data"
