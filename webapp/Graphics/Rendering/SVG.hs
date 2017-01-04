{-# LANGUAGE CPP                #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE ImpredicativeTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.SVG
-- Copyright   :  (c) 2011 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic tools for generating SVG files.
--
-- cf. https://github.com/diagrams/diagrams-svg/blob/v1.3.1.10/src/Graphics/Rendering/SVG.hs
-----------------------------------------------------------------------------
module Graphics.Rendering.SVG
  (SVGFloat
  ,Element
  ,AttributeValue
  ,svgHeader
  ,renderPath
  ,renderText
  ,renderStyles
  ,renderMiterLimit
  ,renderFillTexture
  ,renderLineTexture
  ,getNumAttr)
  where

-- from base
import           Data.List               (intercalate)
#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable           (foldMap)
#endif
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
-- from diagrams-core
import           Diagrams.Core.Transform (matrixHomRep)
-- from diagrams-lib
import           Diagrams.Prelude        hiding (Attribute, Render, with, (<>), stroke, transform, text, width, height)
import           Diagrams.TwoD.Path      (getFillRule)
import           Diagrams.TwoD.Text hiding (text)
-- from text
import           Data.Text               (pack)
import qualified Data.Text               as T
-- from virtual-hom
import           VirtualHom.Element      (Elem, children, content)
import           VirtualHom.Svg hiding (opacity, id)
import qualified VirtualHom.Svg as S
import qualified VirtualHom.Svg.Path as P

-- | Constaint on number type that diagrams-svg can use to render an SVG. This
--   includes the common number types: Double, Float
type SVGFloat n = (Show n, TypeableFloat n)

getNumAttr :: AttributeClass (a n) => (a n -> t) -> Style v n -> Maybe t
getNumAttr f = (f <$>) . getAttr

-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader :: SVGFloat n => forall a. n -> n -> Maybe Element -> [Element -> Element] -> [Element] -> Element
svgHeader w h defines attributes s = svg11 
    & children .~ ds ++ s
    & width (P.toText w)
    & height (P.toText h)
    & font_size "1"
    & viewBox (pack . unwords $ map show ([0, 0, round w, round h] :: [Int]))
    & stroke "rgb(0, 0, 0)"
    & stroke_opacity "1"
    & (appEndo $ foldMap id (fmap Endo attributes))
  where
    ds = maybe mempty return defines

renderPath :: SVGFloat n => Path V2 n -> [Element]
renderPath trs =
    if makePath == T.empty
        then mempty
        else [path_ & d makePath]
  where
    makePath = foldMap renderTrail (op Path trs)

renderTrail :: SVGFloat n => Located (Trail V2 n) -> AttributeValue
renderTrail (viewLoc -> (P (V2 x y),t)) =
    P.mA x y <> withTrail renderLine renderLoop t
  where
    renderLine = foldMap renderSeg . lineSegments
    renderLoop lp =
        case loopSegments lp of
            -- let z handle the last segment if it is linear
            (segs,Linear _) -> foldMap renderSeg segs
            -- otherwise we have to emit it explicitly
            _ -> foldMap renderSeg (lineSegments . cutLoop $ lp) <> P.z

renderSeg :: SVGFloat n => Segment Closed V2 n -> AttributeValue
renderSeg (Linear (OffsetClosed (V2 x 0))) = P.hR x
renderSeg (Linear (OffsetClosed (V2 0 y))) = P.vR y
renderSeg (Linear (OffsetClosed (V2 x y))) = P.lR x y
renderSeg (Cubic (V2 x0 y0) (V2 x1 y1) (OffsetClosed (V2 x2 y2))) =
    P.cR x0 y0 x1 y1 x2 y2

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: SVGFloat n => Int -> Style v n -> [Attribute]
renderFillTexture ident s =
    case getNumAttr getFillTexture s of
        Just (SC (SomeColor c)) ->
            renderTextAttr fill fillColorRgb <>
            renderAttr fill_opacity fillColorOpacity
            where fillColorRgb = Just $ colorToRgbText c
                  fillColorOpacity = Just $ colorToOpacity c
        _ -> []

renderLineTexture :: SVGFloat n => Int -> Style v n -> [Attribute]
renderLineTexture ident s =
    case getNumAttr getLineTexture s of
        Just (SC (SomeColor c)) ->
            renderTextAttr stroke lineColorRgb <>
            renderAttr stroke_opacity lineColorOpacity
            where lineColorRgb = Just $ colorToRgbText c
                  lineColorOpacity = Just $ colorToOpacity c
        _ -> []

renderText :: SVGFloat n => Text n -> Element
renderText (Text tt tAlign str) = text
    & transform transformMatrix
    & dominant_baseline vAlign
    & text_anchor hAlign
    & stroke "none"
    & content .~ T.pack str
  where
    vAlign =
        case tAlign of
            BaselineText -> "alphabetic"
            BoxAlignedText _ h ->
                case h    -- A mere approximation
                      of
                    h'
                      | h' <= 0.25 -> "text-after-edge"
                    h'
                      | h' >= 0.75 -> "text-before-edge"
                    _ -> "middle"
    hAlign =
        case tAlign of
            BaselineText -> "start"
            BoxAlignedText w _ ->
                case w    -- A mere approximation
                      of
                    w'
                      | w' <= 0.25 -> "start"
                    w'
                      | w' >= 0.75 -> "end"
                    _ -> "middle"
    t = tt `mappend` reflectionY
    [[a,b],[c,d],[e,f]] = matrixHomRep t
    transformMatrix = P.matrix a b c d e f

renderStyles :: SVGFloat n => Int -> Int -> Style v n -> [Attribute]
renderStyles fillId lineId s =
    concatMap ($ s) $
    [ renderLineTexture lineId
    , renderFillTexture fillId
    , renderLineWidth
    , renderLineCap
    , renderLineJoin
    , renderFillRule
    , renderDashing
    , renderOpacity
    , renderFontSize
    , renderFontSlant
    , renderFontWeight
    , renderFontFamily
    , renderMiterLimit]

renderMiterLimit :: Style v n -> [Attribute]
renderMiterLimit s = renderAttr stroke_miterlimit miterLimit
  where
    miterLimit = getLineMiterLimit <$> getAttr s

renderOpacity :: Style v n -> [Attribute]
renderOpacity s = renderAttr S.opacity o
  where
    o = getOpacity <$> getAttr s

renderFillRule :: Style v n -> [Attribute]
renderFillRule s = renderTextAttr fill_rule fr
  where
    fr = (fillRuleToText . getFillRule) <$> getAttr s
    fillRuleToText :: FillRule -> AttributeValue
    fillRuleToText Winding = "nonzero"
    fillRuleToText EvenOdd = "evenodd"

renderLineWidth :: SVGFloat n => Style v n -> [Attribute]
renderLineWidth s = renderAttr stroke_width lWidth
  where
    lWidth = getNumAttr getLineWidth s

renderLineCap :: Style v n -> [Attribute]
renderLineCap s = renderTextAttr stroke_linecap lCap
  where
    lCap = (lineCapToText . getLineCap) <$> getAttr s
    lineCapToText :: LineCap -> AttributeValue
    lineCapToText LineCapButt = "butt"
    lineCapToText LineCapRound = "round"
    lineCapToText LineCapSquare = "square"

renderLineJoin :: Style v n -> [Attribute]
renderLineJoin s = renderTextAttr stroke_linejoin lj
  where
    lj = (lineJoinToText . getLineJoin) <$> getAttr s
    lineJoinToText :: LineJoin -> AttributeValue
    lineJoinToText LineJoinMiter = "miter"
    lineJoinToText LineJoinRound = "round"
    lineJoinToText LineJoinBevel = "bevel"

renderDashing :: SVGFloat n => Style v n -> [Attribute]
renderDashing s =
    renderTextAttr stroke_dasharray arr <>
    renderAttr stroke_dashoffset dOffset
  where
    getDasharray (Dashing a _) = a
    getDashoffset (Dashing _ o) = o
    dashArrayToStr = intercalate "," . map show
    -- Ignore dashing if dashing array is empty
    checkEmpty (Just (Dashing [] _)) = Nothing
    checkEmpty other = other
    dashing' = checkEmpty $ getNumAttr getDashing s
    arr = (pack . dashArrayToStr . getDasharray) <$> dashing'
    dOffset = getDashoffset <$> dashing'

renderFontSize :: SVGFloat n => Style v n -> [Attribute]
renderFontSize s = renderTextAttr font_size fs
  where
    fs = pack <$> getNumAttr ((++ "px") . show . getFontSize) s

renderFontSlant :: Style v n -> [Attribute]
renderFontSlant s = renderTextAttr font_style fs
  where
    fs = (fontSlantAttr . getFontSlant) <$> getAttr s
    fontSlantAttr :: FontSlant -> AttributeValue
    fontSlantAttr FontSlantItalic = "italic"
    fontSlantAttr FontSlantOblique = "oblique"
    fontSlantAttr FontSlantNormal = "normal"

renderFontWeight :: Style v n -> [Attribute]
renderFontWeight s = renderTextAttr font_weight fw
  where
    fw = (fontWeightAttr . getFontWeight) <$> getAttr s
    fontWeightAttr :: FontWeight -> AttributeValue
    fontWeightAttr FontWeightNormal = "normal"
    fontWeightAttr FontWeightBold = "bold"

renderFontFamily :: Style v n -> [Attribute]
renderFontFamily s = renderTextAttr font_family ff
  where
    ff = (pack . getFont) <$> getAttr s

-- | Render a style attribute if available, empty otherwise.
renderAttr :: Show s => AttrTag -> Maybe s -> [Attribute]
renderAttr attr valM = maybe [] (\v -> [(bindAttr attr) (pack . show $ v)]) valM

-- renderTextAttr :: (AttributeValue -> Attribute) -> Maybe AttributeValue -> [Attribute]
renderTextAttr :: AttrTag -> Maybe AttributeValue -> [Attribute]
renderTextAttr attr valM = maybe [] (\v -> [(bindAttr attr) v]) valM

colorToRgbText :: forall c. Color c => c -> AttributeValue
colorToRgbText c = T.concat ["rgb(", int r, ",", int g, ",", int b, ")"]
  where
    int d = pack . show $ (round (d * 255) :: Int)
    (r,g,b,_) = colorToSRGBA c

colorToOpacity :: forall c. Color c => c -> Double
colorToOpacity c = a
  where
    (_,_,_,a) = colorToSRGBA c
