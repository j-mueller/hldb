{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.JournalComponent where

import           Control.Lens hiding (children)
import           Data.Accounting.Journal (Journal, totalBalance, unJournal,  firstDay, lastDay, journalMeasure, transactionCount)
import           Data.Semigroup (Option(..), Min(..), Max(..), Sum(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Hledger.Components.Utils (addClassName)
import           VirtualHom.Element 
import qualified VirtualHom.Html as H 
import           VirtualHom.Components

journalComponent :: Component Journal
journalComponent = component' $ \jnl ->
  [H.div 
    & addClassName "journal-container"
    & children .~ [
      H.div 
        & addClassName "focus-panel"
        & children .~ [
          H.div & addClassName "title" & content .~ "Journal overview",
          H.div & addClassName "focus-panel-container" & children .~ [
            makeItem "First entry" $ T.pack $ showFrom $ view (to journalMeasure.firstDay) jnl,
            makeItem "Last entry" $ T.pack $ showTo $ view (to journalMeasure.lastDay) jnl,
            makeItem "Transactions" $ T.pack $ showTxns $ view (to journalMeasure.transactionCount) jnl
          ]
        ]
    ]] where
    makeItem lbl txt = H.div 
      & addClassName "item-card kpi" 
      & children .~ [
        H.div & addClassName "title" & content .~ lbl,
        H.div & addClassName "description" & content .~ txt
      ]
    showFrom t = case t of
      Option (Just (Min d)) -> show d
      _ -> ""
    showTo t = case t of
      Option (Just (Max d)) -> show d
      _ -> ""
    showTxns (Sum i) = show i