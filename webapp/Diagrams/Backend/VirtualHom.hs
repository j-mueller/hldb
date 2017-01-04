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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Diagrams.Backend.VirtualHom where

import           Data.Tree

-- from lens
import           Control.Lens             hiding (transform, ( # ), children)

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
import           VirtualHom.Element       (Elem, children)

-- from this package
import           Graphics.Rendering.SVG   (SVGFloat)
import qualified Graphics.Rendering.SVG   as R
import           VirtualHom.Svg (Attribute, Element)
import qualified VirtualHom.Svg as S
import           VirtualHom.Svg.Path (toText)

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
type SvgRenderM n = ReaderT (Environment n) (State SvgRenderState) [Element]

runRenderM :: SVGFloat n => T.Text -> SvgRenderM n -> [Element]
runRenderM o s = flip evalState initialSvgRenderState
               $ runReaderT s (initialEnvironment o)

instance Monoid (Render VirtualHomSVG V2 n) where
  mempty = R $ return mempty
  R r1 `mappend` R r2_ = R $ do
    svg1 <- r1
    svg2 <- r2_
    return (svg1 `mappend` svg2)

instance SVGFloat n => Backend VirtualHomSVG V2 n where
  newtype Render  VirtualHomSVG V2 n = R (SvgRenderM n)
  type    Result  VirtualHomSVG V2 n = [Element]
  data    Options VirtualHomSVG V2 n = SVGOptions
    { _size            :: SizeSpec V2 n   -- ^ The requested size.
    , _svgDefinitions  :: Maybe Element
                          -- ^ Custom definitions that will be added to the @defs@
                          --   section of the output.
    , _idPrefix        :: T.Text
    , _svgAttributes   :: [Attribute]
                          -- ^ Attriubtes to apply to the entire svg element.
    }
  renderRTree :: VirtualHomSVG -> Options VirtualHomSVG V2 n -> RTree VirtualHomSVG V2 n Annotation -> Result VirtualHomSVG V2 n
  renderRTree _ opts rt = runRenderM (opts ^.idPrefix) svgOutput
    where
      svgOutput = do
        let R r    = rtree (splitTextureFills rt)
            V2 w h = specToSize 100 (opts^.sizeSpec)
        svg <- r
        return $ [R.svgHeader w h (opts^.svgDefinitions) (opts^.svgAttributes) svg]

rtree :: SVGFloat n => RTree VirtualHomSVG V2 n Annotation -> Render VirtualHomSVG V2 n
rtree (Node n rs) = case n of
  RPrim p                 -> render VirtualHomSVG p
  RStyle sty              -> R $ local (over style (<> sty)) r
  RAnnot (OpacityGroup o) -> 
    R $ (r >>= \r' -> return [S.g & S.opacity (toText o) & children .~ r'])
  RAnnot (Href uri)       -> 
    R $ (r >>= \r' -> return [S.a & S.xlinkHref (T.pack uri) & children .~ r'])
  _                       -> R r
  where
    R r = foldMap rtree rs

-- | Lens onto the size of the svg options.
sizeSpec :: Lens' (Options VirtualHomSVG V2 n) (SizeSpec V2 n)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svg definitions of the svg options.
svgDefinitions :: Lens' (Options VirtualHomSVG V2 n) (Maybe Element)
svgDefinitions f opts =
  f (_svgDefinitions opts) <&> \ds -> opts { _svgDefinitions = ds }

-- | Lens onto the idPrefix of the svg options. This is the prefix given
--   to clipping paths to distinguish them from other svg files in the
--   same web page.
idPrefix :: Lens' (Options VirtualHomSVG V2 n) T.Text
idPrefix f opts = f (_idPrefix opts) <&> \i -> opts { _idPrefix = i }

-- | Lens onto the svgAttributes field of the svg options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options VirtualHomSVG V2 n) [Attribute]
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

attributedRender :: SVGFloat n => [Element] -> SvgRenderM n
attributedRender svg = do
  SvgRenderState _idClip idFill idLine <- get
  Environment sty preT <- ask
  let attributes = R.renderStyles idFill idLine sty
  return [S.g 
    & children .~ svg 
    & (appEndo $ foldMap id (fmap Endo attributes))]

instance SVGFloat n => Renderable (Path V2 n) VirtualHomSVG where
  render _ = R . attributedRender . R.renderPath

instance SVGFloat n => Renderable (Text n) VirtualHomSVG where
  render _ = R . attributedRender . return . R.renderText
