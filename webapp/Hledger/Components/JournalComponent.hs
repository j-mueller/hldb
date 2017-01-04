{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.JournalComponent where

import           Control.Lens hiding (children, (#))
import           Data.Accounting.Journal (Journal, totalBalance, unJournal,  firstDay, lastDay, journalMeasure, transactionCount)
import           Data.Semigroup (Option(..), Min(..), Max(..), Sum(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Diagrams.Prelude
import           Graphics.Rendering.SVG (SVGFloat)
import           Hledger.Components.Utils (addClassName)
import           VirtualHom.Element 
import           VirtualHom.Internal.Element (mapCallbacks)
import qualified VirtualHom.Html as H 
import           VirtualHom.Components

import Diagrams.Backend.VirtualHom

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
          ] ++ (fmap (mapCallbacks absurd) rendered)
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

path = fromVertices (map p2 [(0,0), (100,30), (200,0), (220,30)]) # lw 1.5

example :: QDiagram VirtualHomSVG V2 Float Any 
example = path

rendered = renderDia VirtualHomSVG opts example where
  opts = SVGOptions sizeSpec Nothing "" []
  sizeSpec = mkSizeSpec2D (Just 400) Nothing