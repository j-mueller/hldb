{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.JournalLoader where
  
import           Control.Lens hiding (children)
import           Control.Monad.IO.Class
import           Data.Accounting.Journal (Journal)
import           Data.Text (Text)
import           Hledger.Components.Utils (addClassName)
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
      makeDS "Ledger file" "Load a ledger file from disk" 
        & callbacks . click ?~ const selectLedgerFile,
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

selectLedgerFile :: Handler (Maybe Journal -> Maybe Journal) ()
selectLedgerFile = liftIO $ putStrLn "Ledger file"

selectRandomData :: Handler (Maybe Journal -> Maybe Journal) ()
selectRandomData = liftIO $ putStrLn "Random data"