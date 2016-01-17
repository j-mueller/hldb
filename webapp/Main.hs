{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens hiding (children, transform)
import Control.Monad.Fix
import Data.Monoid
import Prelude hiding (div)
import qualified Data.Text as T

import Hledger.Dashboard.Account
import Hledger.Dashboard.Currency
import Hledger.UI.Element
import Hledger.UI.Rendering
import Hledger.UI.Styles.Bootstrap

-- UI of state s should be MonadWriter (Elem ()) m => s -> m s

type View a = forall f. Functor f => a -> ((a -> a) -> f ()) -> Elem f ()

theUI :: View Int
theUI i f = container & children .~ [
    row & children .~ [
      h1 "Hello, world",
      p "I am a paragraph, " & children .~ [strong "too"],
      p ("I have been clicked " <> (T.pack $ show i) <> " times"),
      btnDefault &
        content .~ "Submit" &
        callbacks . onClick ?~ (fmap (const ()) $ f $ \_ -> succ i)]
    ]

renderUI :: RenderingOptions -> View a -> a -> IO ()
renderUI opts f s = do
  putStrLn "Rendering UI"
  let cb g = renderUI opts f (g s)
  let v = f s cb -- v :: Elem IO ()
  render opts Nothing v
  return ()


main :: IO ()
main = do
  let options = RenderingOptions "hldb"
  -- e <- render options Nothing $ theUI 10
  -- putStrLn "Rendering complete"
  renderUI options theUI 0
