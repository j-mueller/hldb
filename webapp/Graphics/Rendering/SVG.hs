{-# LANGUAGE CPP                #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ViewPatterns       #-}

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
  ,renderClip
  ,renderText
  ,renderStyles
  ,renderMiterLimit
  ,renderFillTextureDefs
  ,renderFillTexture
  ,renderLineTextureDefs
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
import           Diagrams.Prelude        hiding (Attribute, Render, with, (<>), stroke, transform, text)
import           Diagrams.TwoD.Path      (getFillRule)
import           Diagrams.TwoD.Text hiding (text)
-- from text
import           Data.Text               (pack)
import qualified Data.Text               as T
-- from virtual-hom
import           VirtualHom.Element      (Elem, children, content)
import           VirtualHom.Svg hiding (opacity)
import qualified VirtualHom.Svg as S

-- | Constaint on number type that diagrams-svg can use to render an SVG. This
--   includes the common number types: Double, Float
type SVGFloat n = (Show n, TypeableFloat n)

-- Could we change Text.Blaze.SVG to use
--   showFFloat :: RealFloat a => Maybe Int -> a -> ShowS
-- or something similar for all numbers so we need TypeableFloat constraint.


getNumAttr
    :: AttributeClass (a n)
    => (a n -> t) -> Style v n -> Maybe t
getNumAttr f = (f <$>) . getAttr

-- | @svgHeader w h defs s@: @w@ width, @h@ height,
--   @defs@ global definitions for defs sections, @s@ actual SVG content.
svgHeader
    :: SVGFloat n
    => n -> n -> Maybe Element -> [Attribute] -> Bool -> Element -> Element
svgHeader w h defines attributes genDoctype s =
    dt <>
    with
        (svg11_ (defs_ [] ds <> s))
        ([ Width_ <<- toText w
         , Height_ <<- toText h
         , Font_size_ <<- "1"
         , ViewBox_ <<-
           (pack . unwords $ map show ([0, 0, round w, round h] :: [Int]))
         , Stroke_ <<- "rgb(0,0,0)"
         , Stroke_opacity_ <<- "1"] ++
         attributes)
  where
    ds = fromMaybe mempty defines
    dt =
        if genDoctype
            then doctype
            else mempty

renderPath
    :: SVGFloat n
    => Path V2 n -> Element
renderPath trs =
    if makePath == T.empty
        then mempty
        else path_ [D_ <<- makePath]
  where
    makePath = foldMap renderTrail (op Path trs)

renderTrail
    :: SVGFloat n
    => Located (Trail V2 n) -> AttributeValue
renderTrail (viewLoc -> (P (V2 x y),t)) =
    mA x y <> withTrail renderLine renderLoop t
  where
    renderLine = foldMap renderSeg . lineSegments
    renderLoop lp =
        case loopSegments lp of
            -- let z handle the last segment if it is linear
            (segs,Linear _) -> foldMap renderSeg segs
            -- otherwise we have to emit it explicitly
            _ -> foldMap renderSeg (lineSegments . cutLoop $ lp) <> z

renderSeg
    :: SVGFloat n
    => Segment Closed V2 n -> AttributeValue
renderSeg (Linear (OffsetClosed (V2 x 0))) = hR x
renderSeg (Linear (OffsetClosed (V2 0 y))) = vR y
renderSeg (Linear (OffsetClosed (V2 x y))) = lR x y
renderSeg (Cubic (V2 x0 y0) (V2 x1 y1) (OffsetClosed (V2 x2 y2))) =
    cR x0 y0 x1 y1 x2 y2

-- TODO: Virtual-hom needs to be changed to not use the id attribute anymore. OR
-- it should allow some kind of stable reference to elements
-- 
-- renderClip :: SVGFloat n => Path V2 n -> T.Text -> Int -> Element -> Element
-- renderClip p prefix ident svg =
--     do defs_ [] $ clipPath_ [Id_ <<- (clipPathId ident)] (renderPath p) <> g_ [Clip_path_ <<- ("url(#" <> clipPathId ident <> ")")] svg
--   where
--     clipPathId i = prefix <> "myClip" <> (pack . show $ i)

-- renderStop :: SVGFloat n => GradientStop n -> Element
-- renderStop (GradientStop c v) =
--     stop_
--         [ Stop_color_ <<- (colorToRgbText c)
--         , Offset_ <<- (toText v)
--         , Stop_opacity_ <<- (toText $ colorToOpacity c)]

-- spreadMethodText :: SpreadMethod -> AttributeValue
-- spreadMethodText GradPad = "pad"
-- spreadMethodText GradReflect = "reflect"
-- spreadMethodText GradRepeat = "repeat"

-- renderLinearGradient :: SVGFloat n => LGradient n -> Int -> Element
-- renderLinearGradient g i =
--     linearGradient_
--         [ Id_ <<- (pack $ "gradient" ++ show i)
--         , X1_ <<- toText x1
--         , Y1_ <<- toText y1
--         , X2_ <<- toText x2
--         , Y2_ <<- toText y2
--         , GradientTransform_ <<- mx
--         , GradientUnits_ <<- "userSpaceOnUse"
--         , SpreadMethod_ <<- spreadMethodText (g ^. lGradSpreadMethod)] $
--     foldMap renderStop (g ^. lGradStops)
--   where
--     mx = matrix a1 a2 b1 b2 c1 c2
--     [[a1,a2],[b1,b2],[c1,c2]] = matrixHomRep (g ^. lGradTrans)
--     P (V2 x1 y1) = g ^. lGradStart
--     P (V2 x2 y2) = g ^. lGradEnd

-- renderRadialGradient :: SVGFloat n => RGradient n -> Int -> Element
-- renderRadialGradient g i = radialGradient 
--     &
--         [ Id_ <<- (pack $ "gradient" ++ show i)
--         , R_ <<- toText (g ^. rGradRadius1)
--         , Cx_ <<- toText cx
--         , Cy_ <<- toText cy
--         , Fx_ <<- toText fx
--         , Fy_ <<- toText fy
--         , GradientTransform_ <<- mx
--         , GradientUnits_ <<- "userSpaceOnUse"
--         , SpreadMethod_ <<- spreadMethodText (g ^. rGradSpreadMethod)]
--         (foldMap renderStop ss)
--   where
--     mx = matrix a1 a2 b1 b2 c1 c2
--     [[a1,a2],[b1,b2],[c1,c2]] = matrixHomRep (g ^. rGradTrans)
--     P (V2 cx cy) = g ^. rGradCenter1
--     P (V2 fx fy) = g ^. rGradCenter0 -- SVGs focal point is our inner center.
--     -- Adjust the stops so that the gradient begins at the perimeter of
--     -- the inner circle (center0, radius0) and ends at the outer circle.
--     r0 = g ^. rGradRadius0
--     r1 = g ^. rGradRadius1
--     stopFracs =
--         r0 / r1 :
--         map
--             (\s ->
--                   (r0 + (s ^. stopFraction) * (r1 - r0)) / r1)
--             (g ^. rGradStops)
--     gradStops =
--         case g ^. rGradStops of
--             [] -> []
--             xs@(x:_) -> x : xs
--     ss =
--         zipWith
--             (\gs sf ->
--                   gs & stopFraction .~ sf)
--             gradStops
--             stopFracs

-- -- Create a gradient element so that it can be used as an attribute value for fill.
-- renderFillTextureDefs :: SVGFloat n => Int -> Style v n -> [Element]
-- renderFillTextureDefs i s =
--     case getNumAttr getFillTexture s of
--         Just (LG g) -> defs & children .~ [renderLinearGradient g i]
--         Just (RG g) -> defs & children .~ [renderRadialGradient g i]
--         _ -> mempty

-- Render the gradient using the id set up in renderFillTextureDefs.
renderFillTexture :: SVGFloat n => Int -> Style v n -> [Attribute]
renderFillTexture ident s =
    case getNumAttr getFillTexture s of
        Just (SC (SomeColor c)) ->
            renderTextAttr fill fillColorRgb <>
            renderAttr fill_opacity fillColorOpacity
            where fillColorRgb = Just $ colorToRgbText c
                  fillColorOpacity = Just $ colorToOpacity c
        Just (LG _) ->
            [ fill ("url(#gradient" <> (pack . show $ ident) <> ")")
            , fill_opacity "1"]
        Just (RG _) ->
            [ fill ("url(#gradient" <> (pack . show $ ident) <> ")")
            , fill_opacity "1"]
        Nothing -> []

renderLineTextureDefs :: SVGFloat n => Int -> Style v n -> [Element]
renderLineTextureDefs i s =
    case getNumAttr getLineTexture s of
        Just (LG g) -> [defs & children .~ [renderLinearGradient g i]]
        Just (RG g) -> [defs & children .~ [renderRadialGradient g i]]
        _ -> mempty

renderLineTexture :: SVGFloat n => Int -> Style v n -> [Attribute]
renderLineTexture ident s =
    case getNumAttr getLineTexture s of
        Just (SC (SomeColor c)) ->
            renderTextAttr stroke lineColorRgb <>
            renderAttr stroke_opacity lineColorOpacity
            where lineColorRgb = Just $ colorToRgbText c
                  lineColorOpacity = Just $ colorToOpacity c
        Just (LG _) ->
            [ stroke ("url(#gradient" <> (pack . show $ ident) <> ")")
            , stroke_opacity "1"]
        Just (RG _) ->
            [ stroke ("url(#gradient" <> (pack . show $ ident) <> ")")
            , stroke_opacity "1"]
        Nothing -> []

renderText :: SVGFloat n => Text n -> Element
renderText (Text tt tAlign str) = text
    & transform transformMatrix
    & dominant_baseline vAlign
    & text_anchor hAlign
    & stroke "none"
    & content .~ str
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
    transformMatrix = matrix a b c d e f

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
renderOpacity s = renderAttr opacity o
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
