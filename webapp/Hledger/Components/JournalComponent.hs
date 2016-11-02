{-# LANGUAGE OverloadedStrings #-}
module Hledger.Components.JournalComponent where

import           Control.Lens hiding (children)
import           Data.Accounting.Journal (Journal)
import           Data.Text (Text)
import           Hledger.Components.Utils (addClassName)
import           VirtualHom.Element 
import qualified VirtualHom.Html as H 
import           VirtualHom.Components

journalComponent :: Component Journal
journalComponent = component' $ \_ ->
  [H.div & content .~ "has journal"]