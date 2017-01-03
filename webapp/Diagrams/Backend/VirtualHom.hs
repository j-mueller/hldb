{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Diagrams.Backend.VirtualHom where

import           Data.Tree

-- from lens
import           Control.Lens             hiding (transform, ( # ))

-- from base
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Typeable

-- from text
import qualified Data.Text                as T

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types      (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude         hiding (Attribute, local, size, view)
import           Diagrams.TwoD.Adjust     (adjustDia2D)
import           Diagrams.TwoD.Attributes (splitTextureFills)
import           Diagrams.TwoD.Path       (Clip (Clip))
import           Diagrams.TwoD.Text

-- from virtual-hom
import           VirtualHom.Element       (Elem)

-- from this package
import           Graphics.Rendering.SVG   (SVGFloat)
import qualified Graphics.Rendering.SVG   as R

data VirtualHomSVG = VirtualHomSVG
  deriving (Show, Typeable)

type instance V VirtualHomSVG = V2
type instance N VirtualHomSVG = Double


data Environment n = Environment
  { _style :: Style V2 n
  , __pre  :: T.Text
  }

makeLenses ''Environment

data SvgRenderState = SvgRenderState
  { _clipPathId :: Int
  , _fillGradId :: Int
  , _lineGradId :: Int
  }

makeLenses ''SvgRenderState

initialEnvironment :: SVGFloat n => T.Text -> Environment n
initialEnvironment = Environment (mempty # recommendFillColor transparent)

-- Fill gradients ids are even, line gradient ids are odd.
initialSvgRenderState :: SvgRenderState
initialSvgRenderState = SvgRenderState 0 0 1

-- | Monad to keep track of environment and state when rendering an SVG.
type SvgRenderM n = ReaderT (Environment n) (State SvgRenderState) Elem

runRenderM :: SVGFloat n => T.Text -> SvgRenderM n -> Elem
runRenderM o s = flip evalState initialSvgRenderState
               $ runReaderT  s (initialEnvironment o)

instance Monoid (Render VirtualHomSVG V2 n) where
  mempty = R $ return mempty
  R r1 `mappend` R r2_ = R $ do
    svg1 <- r1
    svg2 <- r2_
    return (svg1 `mappend` svg2)

-- Handle clip attributes.
--
renderSvgWithClipping :: forall n. SVGFloat n
                      => T.Text
                      -> Elem       -- ^ Input SVG
                      -> Style V2 n    -- ^ Styles
                      -> SvgRenderM n  -- ^ Resulting svg

renderSvgWithClipping prefix svg s =
  case op Clip <$> getAttr s of
    Nothing    -> return svg
    Just paths -> renderClips paths
  where
    renderClips :: [Path V2 n] -> SvgRenderM n
    renderClips []     = return svg
    renderClips (p:ps) = do
      clipPathId += 1
      ident <- use clipPathId
      R.renderClip p prefix ident <$> renderClips ps

-- | Create a new texture defs svg element using the style and the current
--   id number, then increment the gradient id number.
fillTextureDefs :: SVGFloat n => Style v n -> SvgRenderM n
fillTextureDefs s = do
  ident <- use fillGradId
  fillGradId += 2 -- always even
  return $ R.renderFillTextureDefs ident s

lineTextureDefs :: SVGFloat n => Style v n -> SvgRenderM n
lineTextureDefs s = do
  ident <- use lineGradId
  lineGradId += 2 -- always odd
  return $ R.renderLineTextureDefs ident s

instance SVGFloat n => Backend VirtualHomSVG V2 n where
  newtype Render  VirtualHomSVG V2 n = R (SvgRenderM n)
  type    Result  VirtualHomSVG V2 n = forall a. Elem a ()
  data    Options VirtualHomSVG V2 n = SVGOptions
    { _size            :: SizeSpec V2 n   -- ^ The requested size.
    , _svgDefinitions  :: forall a. Maybe (Elem a ())
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    , _idPrefix        :: T.Text
    , _svgAttributes   :: [Attribute]
                          -- ^ Attriubtes to apply to the entire svg element.
    , _generateDoctype :: Bool
}
