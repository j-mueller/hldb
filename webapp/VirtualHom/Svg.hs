{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ImpredicativeTypes #-}
module VirtualHom.Svg where

import           Data.Void

import           VirtualHom.Internal.Element hiding (content, input, select)
import qualified VirtualHom.Internal.Element as E
import           VirtualHom.Svg.Path (toText)

import           Control.Lens                hiding (element, pre)
import           Data.Text                   (Text)
import qualified Data.Text as T
import           Data.Text.Lazy                   (toStrict)
import           Data.Text.Lazy.Builder           (toLazyText)
import           Data.Text.Lazy.Builder.RealFloat
import           Prelude                     hiding (div, span)

-- SVG Elements
-- TODO: These should really live in the virtual-hom package, not here..
type Element = Elem Void ()

type Attribute = Element -> Element

type AttributeValue = T.Text

type AttrTag = AttributeValue -> Attribute

bindAttr :: AttrTag -> AttributeValue -> Attribute
bindAttr tg v = tg v

makeAttribute :: Text -> Text -> Attribute
makeAttribute k v elm = elm & attributes . at k ?~ v

makeSvgElem :: Text -> Element
makeSvgElem tpe = elm tpe 
  & namespace .~ "http://www.w3.org/2000/svg"

-- | The @accentHeight@ attribute.
accent_height :: AttrTag
accent_height = makeAttribute "accent-height"

-- | The @accumulate@ attribute.
accumulate :: AttrTag
accumulate = makeAttribute "accumulate"

-- | The @additive@ attribute.
additive :: AttrTag
additive = makeAttribute "additive"

-- | The @alignmentBaseline@ attribute.
alignment_baseline :: AttrTag
alignment_baseline = makeAttribute "alignment-baseline"

-- | The @alphabetic@ attribute.
alphabetic :: AttrTag
alphabetic = makeAttribute "alphabetic"

-- | The @amplitude@ attribute.
amplitude :: AttrTag
amplitude = makeAttribute "amplitude"

-- | The @arabicForm@ attribute.
arabic_form :: AttrTag
arabic_form = makeAttribute "arabic-form"

-- | The @ascent@ attribute.
ascent :: AttrTag
ascent = makeAttribute "ascent"

-- | The @attributename@ attribute.
attributeName :: AttrTag
attributeName = makeAttribute "attributeName"

-- | The @attributetype@ attribute.
attributeType :: AttrTag
attributeType = makeAttribute "attributeType"

-- | The @azimuth@ attribute.
azimuth :: AttrTag
azimuth = makeAttribute "azimuth"

-- | The @basefrequency@ attribute.
baseFrequency :: AttrTag
baseFrequency = makeAttribute "baseFrequency"

-- | The @baseprofile@ attribute.
baseprofile :: AttrTag
baseprofile = makeAttribute "baseprofile"

-- | The @baselineShift@ attribute.
baseline_shift :: AttrTag
baseline_shift = makeAttribute "baseline-shift"

-- | The @bbox@ attribute.
bbox :: AttrTag
bbox = makeAttribute "bbox"

-- | The @begin@ attribute.
begin :: AttrTag
begin = makeAttribute "begin"

-- | The @bias@ attribute.
bias :: AttrTag
bias = makeAttribute "bias"

-- | The @by@ attribute.
by :: AttrTag
by = makeAttribute "by"

-- | The @calcmode@ attribute.
calcMode :: AttrTag
calcMode = makeAttribute "calcMode"

-- | The @capHeight@ attribute.
cap_height :: AttrTag
cap_height = makeAttribute "cap-height"

-- | The @class@ attribute.
class_ :: AttrTag
class_ = makeAttribute "class"

-- | The @clip@ attribute.
clip :: AttrTag
clip = makeAttribute "clip"

-- | The @clip-path@ attribute.
clip_path :: AttrTag
clip_path = makeAttribute "clip-path"

-- | The @clipRule@ attribute.
clip_rule :: AttrTag
clip_rule = makeAttribute "clip-rule"

-- | The @clippathunits@ attribute.
clipPathUnits :: AttrTag
clipPathUnits = makeAttribute "clipPathUnits"

-- | The @color@ attribute.
color :: AttrTag
color = makeAttribute "color"

-- | The @colorInterpolation@ attribute.
color_interpolation :: AttrTag
color_interpolation = makeAttribute "color-interpolation"

-- | The @colorInterpolationFilters@ attribute.
color_interpolation_filters :: AttrTag
color_interpolation_filters = makeAttribute "color-interpolation-filters"

-- | The @colorProfile@ attribute.
color_profile :: AttrTag
color_profile = makeAttribute "color-profile"

-- | The @colorRendering@ attribute.
color_rendering :: AttrTag
color_rendering = makeAttribute "color-rendering"

-- | The @contentscripttype@ attribute.
contentScriptType :: AttrTag
contentScriptType = makeAttribute "contentScriptType"

-- | The @contentstyletype@ attribute.
contentStyleType :: AttrTag
contentStyleType = makeAttribute "contentStyleType"

-- | The @cursor@ attribute.
cursor :: AttrTag
cursor = makeAttribute "cursor"

-- | The @cx@ attribute.
cx :: AttrTag
cx = makeAttribute "cx"

-- | The @cy@ attribute.
cy :: AttrTag
cy = makeAttribute "cy"

-- | The @d@ attribute.
d :: AttrTag
d = makeAttribute "d"

-- | The @descent@ attribute.
descent :: AttrTag
descent = makeAttribute "descent"

-- | The @diffuseconstant@ attribute.
diffuseConstant :: AttrTag
diffuseConstant = makeAttribute "diffuseConstant"

-- | The @direction@ attribute.
direction :: AttrTag
direction = makeAttribute "direction"

-- | The @display@ attribute.
display :: AttrTag
display = makeAttribute "display"

-- | The @divisor@ attribute.
divisor :: AttrTag
divisor = makeAttribute "divisor"

-- | The @dominantBaseline@ attribute.
dominant_baseline :: AttrTag
dominant_baseline = makeAttribute "dominant-baseline"

-- | The @dur@ attribute.
dur :: AttrTag
dur = makeAttribute "dur"

-- | The @dx@ attribute.
dx :: AttrTag
dx = makeAttribute "dx"

-- | The @dy@ attribute.
dy :: AttrTag
dy = makeAttribute "dy"

-- | The @edgemode@ attribute.
edgeMode :: AttrTag
edgeMode = makeAttribute "edgeMode"

-- | The @elevation@ attribute.
elevation :: AttrTag
elevation = makeAttribute "elevation"

-- | The @enableBackground@ attribute.
enable_background :: AttrTag
enable_background = makeAttribute "enable-background"

-- | The @end@ attribute.
end :: AttrTag
end = makeAttribute "end"

-- | The @exponent@ attribute.
exponent :: AttrTag
exponent = makeAttribute "exponent"

-- | The @externalresourcesrequired@ attribute.
externalResourcesRequired :: AttrTag
externalResourcesRequired = makeAttribute "externalResourcesRequired"

-- | The @fill@ attribute.
fill :: AttrTag
fill = makeAttribute "fill"

-- | The @fillOpacity@ attribute.
fill_opacity :: AttrTag
fill_opacity = makeAttribute "fill-opacity"

-- | The @fillRule@ attribute.
fill_rule :: AttrTag
fill_rule = makeAttribute "fill-rule"

-- | The @filter@ attribute.
filter :: AttrTag
filter = makeAttribute "filter"

-- | The @filterres@ attribute.
filterRes :: AttrTag
filterRes = makeAttribute "filterRes"

-- | The @filterunits@ attribute.
filterUnits :: AttrTag
filterUnits = makeAttribute "filterUnits"

-- | The @floodColor@ attribute.
flood_color :: AttrTag
flood_color = makeAttribute "flood-color"

-- | The @floodOpacity@ attribute.
flood_opacity :: AttrTag
flood_opacity = makeAttribute "flood-opacity"

-- | The @fontFamily@ attribute.
font_family :: AttrTag
font_family = makeAttribute "font-family"

-- | The @fontSize@ attribute.
font_size :: AttrTag
font_size = makeAttribute "font-size"

-- | The @fontSizeAdjust@ attribute.
font_size_adjust :: AttrTag
font_size_adjust = makeAttribute "font-size-adjust"

-- | The @fontStretch@ attribute.
font_stretch :: AttrTag
font_stretch = makeAttribute "font-stretch"

-- | The @fontStyle@ attribute.
font_style :: AttrTag
font_style = makeAttribute "font-style"

-- | The @fontVariant@ attribute.
font_variant :: AttrTag
font_variant = makeAttribute "font-variant"

-- | The @fontWeight@ attribute.
font_weight :: AttrTag
font_weight = makeAttribute "font-weight"

-- | The @format@ attribute.
format :: AttrTag
format = makeAttribute "format"

-- | The @from@ attribute.
from :: AttrTag
from = makeAttribute "from"

-- | The @fx@ attribute.
fx :: AttrTag
fx = makeAttribute "fx"

-- | The @fy@ attribute.
fy :: AttrTag
fy = makeAttribute "fy"

-- | The @g1@ attribute.
g1 :: AttrTag
g1 = makeAttribute "g1"

-- | The @g2@ attribute.
g2 :: AttrTag
g2 = makeAttribute "g2"

-- | The @glyphName@ attribute.
glyph_name :: AttrTag
glyph_name = makeAttribute "glyph-name"

-- | The @glyphOrientationHorizontal@ attribute.
glyph_orientation_horizontal :: AttrTag
glyph_orientation_horizontal = makeAttribute "glyph-orientation-horizontal"

-- | The @glyphOrientationVertical@ attribute.
glyph_orientation_vertical :: AttrTag
glyph_orientation_vertical = makeAttribute "glyph-orientation-vertical"

-- | The @-- | The @gradienttransform@ attribute.
gradientTransform :: AttrTag
gradientTransform = makeAttribute "gradientTransform"

-- | The @gradientunits@ attribute.
gradientUnits :: AttrTag
gradientUnits = makeAttribute "gradientUnits"

-- | The @hanging@ attribute.
hanging :: AttrTag
hanging = makeAttribute "hanging"

-- | The @height@ attribute.
height :: AttrTag
height = makeAttribute "height"

-- | The @horizAdvX@ attribute.
horiz_adv_x :: AttrTag
horiz_adv_x = makeAttribute "horiz-adv-x"

-- | The @horizOriginX@ attribute.
horiz_origin_x :: AttrTag
horiz_origin_x = makeAttribute "horiz-origin-x"

-- | The @horizOriginY@ attribute.
horiz_origin_y :: AttrTag
horiz_origin_y = makeAttribute "horiz-origin-y"

-- | The @id@ attribute.
id :: AttrTag
id = makeAttribute "id"

-- | The @ideographic@ attribute.
ideographic :: AttrTag
ideographic = makeAttribute "ideographic"

-- | The @imageRendering@ attribute.
image_rendering :: AttrTag
image_rendering = makeAttribute "image-rendering"

-- | The @in@ attribute.
in_ :: AttrTag
in_ = makeAttribute "in"

-- | The @in2@ attribute.
in2 :: AttrTag
in2 = makeAttribute "in2"

-- | The @intercept@ attribute.
intercept :: AttrTag
intercept = makeAttribute "intercept"

-- | The @k@ attribute.
k :: AttrTag
k = makeAttribute "k"

-- | The @k1@ attribute.
k1 :: AttrTag
k1 = makeAttribute "k1"

-- | The @k2@ attribute.
k2 :: AttrTag
k2 = makeAttribute "k2"

-- | The @k3@ attribute.
k3 :: AttrTag
k3 = makeAttribute "k3"

-- | The @k4@ attribute.
k4 :: AttrTag
k4 = makeAttribute "k4"

-- | The @kernelmatrix@ attribute.
kernelMatrix :: AttrTag
kernelMatrix = makeAttribute "kernelMatrix"

-- | The @kernelunitlength@ attribute.
kernelUnitLength :: AttrTag
kernelUnitLength = makeAttribute "kernelUnitLength"

-- | The @kerning@ attribute.
kerning :: AttrTag
kerning = makeAttribute "kerning"

-- | The @keypoints@ attribute.
keyPoints :: AttrTag
keyPoints = makeAttribute "keyPoints"

-- | The @keysplines@ attribute.
keySplines :: AttrTag
keySplines = makeAttribute "keySplines"

-- | The @keytimes@ attribute.
keyTimes :: AttrTag
keyTimes = makeAttribute "keyTimes"

-- | The @lang@ attribute.
lang :: AttrTag
lang = makeAttribute "lang"

-- | The @lengthadjust@ attribute.
lengthAdjust :: AttrTag
lengthAdjust = makeAttribute "lengthAdjust"

-- | The @letterSpacing@ attribute.
letter_spacing :: AttrTag
letter_spacing = makeAttribute "letter-spacing"

-- | The @lightingColor@ attribute.
lighting_color :: AttrTag
lighting_color = makeAttribute "lighting-color"

-- | The @limitingconeangle@ attribute.
limitingConeAngle :: AttrTag
limitingConeAngle = makeAttribute "limitingConeAngle"

-- | The @local@ attribute.
local :: AttrTag
local = makeAttribute "local"

-- | The @markerEnd@ attribute.
marker_end :: AttrTag
marker_end = makeAttribute "marker-end"

-- | The @markerMid@ attribute.
marker_mid :: AttrTag
marker_mid = makeAttribute "marker-mid"

-- | The @markerStart@ attribute.
marker_start :: AttrTag
marker_start = makeAttribute "marker-start"

-- | The @markerheight@ attribute.
markerHeight :: AttrTag
markerHeight = makeAttribute "markerHeight"

-- | The @markerunits@ attribute.
markerUnits :: AttrTag
markerUnits = makeAttribute "markerUnits"

-- | The @markerwidth@ attribute.
markerWidth :: AttrTag
markerWidth = makeAttribute "markerWidth"

-- | The @maskcontentunits@ attribute.
maskContentUnits :: AttrTag
maskContentUnits = makeAttribute "maskContentUnits"

-- | The @maskunits@ attribute.
maskUnits :: AttrTag
maskUnits = makeAttribute "maskUnits"

-- | The @mathematical@ attribute.
mathematical :: AttrTag
mathematical = makeAttribute "mathematical"

-- | The @max@ attribute.
max :: AttrTag
max = makeAttribute "max"

-- | The @media@ attribute.
media :: AttrTag
media = makeAttribute "media"

-- | The @method@ attribute.
method :: AttrTag
method = makeAttribute "method"

-- | The @min@ attribute.
min :: AttrTag
min = makeAttribute "min"

-- | The @mode@ attribute.
mode :: AttrTag
mode = makeAttribute "mode"

-- | The @name@ attribute.
name :: AttrTag
name = makeAttribute "name"

-- | The @numoctaves@ attribute.
numOctaves :: AttrTag
numOctaves = makeAttribute "numOctaves"

-- | The @offset@ attribute.
offset :: AttrTag
offset = makeAttribute "offset"

-- | The @onabort@ attribute.
onabort :: AttrTag
onabort = makeAttribute "onabort"

-- | The @onactivate@ attribute.
onactivate :: AttrTag
onactivate = makeAttribute "onactivate"

-- | The @onbegin@ attribute.
onbegin :: AttrTag
onbegin = makeAttribute "onbegin"

-- | The @onclick@ attribute.
onclick :: AttrTag
onclick = makeAttribute "onclick"

-- | The @onend@ attribute.
onend :: AttrTag
onend = makeAttribute "onend"

-- | The @onerror@ attribute.
onerror :: AttrTag
onerror = makeAttribute "onerror"

-- | The @onfocusin@ attribute.
onfocusin :: AttrTag
onfocusin = makeAttribute "onfocusin"

-- | The @onfocusout@ attribute.
onfocusout :: AttrTag
onfocusout = makeAttribute "onfocusout"

-- | The @onload@ attribute.
onload :: AttrTag
onload = makeAttribute "onload"

-- | The @onmousedown@ attribute.
onmousedown :: AttrTag
onmousedown = makeAttribute "onmousedown"

-- | The @onmousemove@ attribute.
onmousemove :: AttrTag
onmousemove = makeAttribute "onmousemove"

-- | The @onmouseout@ attribute.
onmouseout :: AttrTag
onmouseout = makeAttribute "onmouseout"

-- | The @onmouseover@ attribute.
onmouseover :: AttrTag
onmouseover = makeAttribute "onmouseover"

-- | The @onmouseup@ attribute.
onmouseup :: AttrTag
onmouseup = makeAttribute "onmouseup"

-- | The @onrepeat@ attribute.
onrepeat :: AttrTag
onrepeat = makeAttribute "onrepeat"

-- | The @onresize@ attribute.
onresize :: AttrTag
onresize = makeAttribute "onresize"

-- | The @onscroll@ attribute.
onscroll :: AttrTag
onscroll = makeAttribute "onscroll"

-- | The @onunload@ attribute.
onunload :: AttrTag
onunload = makeAttribute "onunload"

-- | The @onzoom@ attribute.
onzoom :: AttrTag
onzoom = makeAttribute "onzoom"

-- | The @opacity@ attribute.
opacity :: AttrTag
opacity = makeAttribute "opacity"

-- | The @operator@ attribute.
operator :: AttrTag
operator = makeAttribute "operator"

-- | The @order@ attribute.
order :: AttrTag
order = makeAttribute "order"

-- | The @orient@ attribute.
orient :: AttrTag
orient = makeAttribute "orient"

-- | The @orientation@ attribute.
orientation :: AttrTag
orientation = makeAttribute "orientation"

-- | The @origin@ attribute.
origin :: AttrTag
origin = makeAttribute "origin"

-- | The @overflow@ attribute.
overflow :: AttrTag
overflow = makeAttribute "overflow"

-- | The @overlinePosition@ attribute.
overline_position :: AttrTag
overline_position = makeAttribute "overline-position"

-- | The @overlineThickness@ attribute.
overline_thickness :: AttrTag
overline_thickness = makeAttribute "overline-thickness"

-- | The @panose1@ attribute.
panose_1 :: AttrTag
panose_1 = makeAttribute "panose-1"

-- | The @paint-order@ attribute.
paint_order :: AttrTag
paint_order = makeAttribute "paint-order"

-- | The @path@ attribute.
path :: AttrTag
path = makeAttribute "path"

-- | The @pathlength@ attribute.
pathLength :: AttrTag
pathLength = makeAttribute "pathLength"

-- | The @patterncontentunits@ attribute.
patternContentUnits :: AttrTag
patternContentUnits = makeAttribute "patternContentUnits"

-- | The @patterntransform@ attribute.
patternTransform :: AttrTag
patternTransform = makeAttribute "patternTransform"

-- | The @patternunits@ attribute.
patternUnits :: AttrTag
patternUnits = makeAttribute "patternUnits"

-- | The @pointerEvents@ attribute.
pointer_events :: AttrTag
pointer_events = makeAttribute "pointer-events"

-- | The @points@ attribute.
points :: AttrTag
points = makeAttribute "points"

-- | The @pointsatx@ attribute.
pointsAtX :: AttrTag
pointsAtX = makeAttribute "pointsAtX"

-- | The @pointsaty@ attribute.
pointsAtY :: AttrTag
pointsAtY = makeAttribute "pointsAtY"

-- | The @pointsatz@ attribute.
pointsAtZ :: AttrTag
pointsAtZ = makeAttribute "pointsAtZ"

-- | The @preservealpha@ attribute.
preserveAlpha :: AttrTag
preserveAlpha = makeAttribute "preserveAlpha"

-- | The @preserveaspectratio@ attribute.
preserveAspectRatio :: AttrTag
preserveAspectRatio = makeAttribute "preserveAspectRatio"

-- | The @primitiveunits@ attribute.
primitiveUnits :: AttrTag
primitiveUnits = makeAttribute "primitiveUnits"

-- | The @r@ attribute.
r :: AttrTag
r = makeAttribute "r"

-- | The @radius@ attribute.
radius :: AttrTag
radius = makeAttribute "radius"

-- | The @refx@ attribute.
refX :: AttrTag
refX = makeAttribute "refX"

-- | The @refy@ attribute.
refY :: AttrTag
refY = makeAttribute "refY"

-- | The @renderingIntent@ attribute.
rendering_intent :: AttrTag
rendering_intent = makeAttribute "rendering-intent"

-- | The @repeatcount@ attribute.
repeatCount :: AttrTag
repeatCount = makeAttribute "repeatCount"

-- | The @repeatdur@ attribute.
repeatDur :: AttrTag
repeatDur = makeAttribute "repeatDur"

-- | The @requiredextensions@ attribute.
requiredExtensions :: AttrTag
requiredExtensions = makeAttribute "requiredExtensions"

-- | The @requiredfeatures@ attribute.
requiredFeatures :: AttrTag
requiredFeatures = makeAttribute "requiredFeatures"

-- | The @restart@ attribute.
restart :: AttrTag
restart = makeAttribute "restart"

-- | The @result@ attribute.
result :: AttrTag
result = makeAttribute "result"

-- | The @rotate@ attribute.
rotate :: AttrTag
rotate = makeAttribute "rotate"

-- | The @rx@ attribute.
rx :: AttrTag
rx = makeAttribute "rx"

-- | The @ry@ attribute.
ry :: AttrTag
ry = makeAttribute "ry"

-- | The @scale@ attribute.
scale :: AttrTag
scale = makeAttribute "scale"

-- | The @seed@ attribute.
seed :: AttrTag
seed = makeAttribute "seed"

-- | The @shapeRendering@ attribute.
shape_rendering :: AttrTag
shape_rendering = makeAttribute "shape-rendering"

-- | The @slope@ attribute.
slope :: AttrTag
slope = makeAttribute "slope"

-- | The @spacing@ attribute.
spacing :: AttrTag
spacing = makeAttribute "spacing"

-- | The @specularconstant@ attribute.
specularConstant :: AttrTag
specularConstant = makeAttribute "specularConstant"

-- | The @specularexponent@ attribute.
specularExponent :: AttrTag
specularExponent = makeAttribute "specularExponent"

-- | The @spreadmethod@ attribute.
spreadMethod :: AttrTag
spreadMethod = makeAttribute "spreadMethod"

-- | The @startoffset@ attribute.
startOffset :: AttrTag
startOffset = makeAttribute "startOffset"

-- | The @stddeviation@ attribute.
stdDeviation :: AttrTag
stdDeviation = makeAttribute "stdDeviation"

-- | The @stemh@ attribute.
stemh :: AttrTag
stemh = makeAttribute "stemh"

-- | The @stemv@ attribute.
stemv :: AttrTag
stemv = makeAttribute "stemv"

-- | The @stitchtiles@ attribute.
stitchTiles :: AttrTag
stitchTiles = makeAttribute "stitchTiles"

-- | The @stopColor@ attribute.
stop_color :: AttrTag
stop_color = makeAttribute "stop-color"

-- | The @stopOpacity@ attribute.
stop_opacity :: AttrTag
stop_opacity = makeAttribute "stop-opacity"

-- | The @strikethroughPosition@ attribute.
strikethrough_position :: AttrTag
strikethrough_position = makeAttribute "strikethrough-position"

-- | The @strikethroughThickness@ attribute.
strikethrough_thickness :: AttrTag
strikethrough_thickness = makeAttribute "strikethrough-thickness"

-- | The @string@ attribute.
string :: AttrTag
string = makeAttribute "string"

-- | The @stroke@ attribute.
stroke :: AttrTag
stroke = makeAttribute "stroke"

-- | The @strokeDasharray@ attribute.
stroke_dasharray :: AttrTag
stroke_dasharray = makeAttribute "stroke-dasharray"

-- | The @strokeDashoffset@ attribute.
stroke_dashoffset :: AttrTag
stroke_dashoffset = makeAttribute "stroke-dashoffset"

-- | The @strokeLinecap@ attribute.
stroke_linecap :: AttrTag
stroke_linecap = makeAttribute "stroke-linecap"

-- | The @strokeLinejoin@ attribute.
stroke_linejoin :: AttrTag
stroke_linejoin = makeAttribute "stroke-linejoin"

-- | The @strokeMiterlimit@ attribute.
stroke_miterlimit :: AttrTag
stroke_miterlimit = makeAttribute "stroke-miterlimit"

-- | The @strokeOpacity@ attribute.
stroke_opacity :: AttrTag
stroke_opacity = makeAttribute "stroke-opacity"

-- | The @strokeWidth@ attribute.
stroke_width :: AttrTag
stroke_width = makeAttribute "stroke-width"

-- | The @style@ attribute.
style :: AttrTag
style = makeAttribute "style"

-- | The @surfacescale@ attribute.
surfaceScale :: AttrTag
surfaceScale = makeAttribute "surfaceScale"

-- | The @systemlanguage@ attribute.
systemLanguage :: AttrTag
systemLanguage = makeAttribute "systemLanguage"

-- | The @tablevalues@ attribute.
tableValues :: AttrTag
tableValues = makeAttribute "tableValues"

-- | The @target@ attribute.
target :: AttrTag
target = makeAttribute "target"

-- | The @targetx@ attribute.
targetX :: AttrTag
targetX = makeAttribute "targetX"

-- | The @targety@ attribute.
targetY :: AttrTag
targetY = makeAttribute "targetY"

-- | The @textAnchor@ attribute.
text_anchor :: AttrTag
text_anchor = makeAttribute "text-anchor"

-- | The @textDecoration@ attribute.
text_decoration :: AttrTag
text_decoration = makeAttribute "text-decoration"

-- | The @textRendering@ attribute.
text_rendering :: AttrTag
text_rendering = makeAttribute "text-rendering"

-- | The @textlength@ attribute.
textLength :: AttrTag
textLength = makeAttribute "textLength"

-- | The @to@ attribute.
to :: AttrTag
to = makeAttribute "to"

-- | The @transform@ attribute.
transform :: AttrTag
transform = makeAttribute "transform"

-- | The @type@ attribute.
type_ :: AttrTag
type_ = makeAttribute "type"

-- | The @u1@ attribute.
u1 :: AttrTag
u1 = makeAttribute "u1"

-- | The @u2@ attribute.
u2 :: AttrTag
u2 = makeAttribute "u2"

-- | The @underlinePosition@ attribute.
underline_position :: AttrTag
underline_position = makeAttribute "underline-position"

-- | The @underlineThickness@ attribute.
underline_thickness :: AttrTag
underline_thickness = makeAttribute "underline-thickness"

-- | The @unicode@ attribute.
unicode :: AttrTag
unicode = makeAttribute "unicode"

-- | The @unicodeBidi@ attribute.
unicode_bidi :: AttrTag
unicode_bidi = makeAttribute "unicode-bidi"

-- | The @unicodeRange@ attribute.
unicode_range :: AttrTag
unicode_range = makeAttribute "unicode-range"

-- | The @unitsPerEm@ attribute.
units_per_em :: AttrTag
units_per_em = makeAttribute "units-per-em"

-- | The @vAlphabetic@ attribute.
v_alphabetic :: AttrTag
v_alphabetic = makeAttribute "v-alphabetic"

-- | The @vHanging@ attribute.
v_hanging :: AttrTag
v_hanging = makeAttribute "v-hanging"

-- | The @vIdeographic@ attribute.
v_ideographic :: AttrTag
v_ideographic = makeAttribute "v-ideographic"

-- | The @vMathematical@ attribute.
v_mathematical :: AttrTag
v_mathematical = makeAttribute "v-mathematical"

-- | The @values@ attribute.
values :: AttrTag
values = makeAttribute "values"

-- | The @version@ attribute.
version :: AttrTag
version = makeAttribute "version"

-- | The @vertAdvY@ attribute.
vert_adv_y :: AttrTag
vert_adv_y = makeAttribute "vert-adv-y"

-- | The @vertOriginX@ attribute.
vert_origin_x :: AttrTag
vert_origin_x = makeAttribute "vert-origin-x"

-- | The @vertOriginY@ attribute.
vert_origin_y :: AttrTag
vert_origin_y = makeAttribute "vert-origin-y"

-- | The @viewbox@ attribute.
viewBox :: AttrTag
viewBox = makeAttribute "viewBox"

-- | The @viewtarget@ attribute.
viewTarget :: AttrTag
viewTarget = makeAttribute "viewTarget"

-- | The @visibility@ attribute.
visibility :: AttrTag
visibility = makeAttribute "visibility"

-- | The @width@ attribute.
width :: AttrTag
width = makeAttribute "width"

-- | The @widths@ attribute.
widths :: AttrTag
widths = makeAttribute "widths"

-- | The @wordSpacing@ attribute.
word_spacing :: AttrTag
word_spacing = makeAttribute "word-spacing"

-- | The @writingMode@ attribute.
writing_mode :: AttrTag
writing_mode = makeAttribute "writing-mode"

-- | The @x@ attribute.
x :: AttrTag
x = makeAttribute "x"

-- | The @xHeight@ attribute.
x_height :: AttrTag
x_height = makeAttribute "x-height"

-- | The @x1@ attribute.
x1 :: AttrTag
x1 = makeAttribute "x1"

-- | The @x2@ attribute.
x2 :: AttrTag
x2 = makeAttribute "x2"

-- | The @xchannelselector@ attribute.
xChannelSelector :: AttrTag
xChannelSelector = makeAttribute "xChannelSelector"

-- | The @xlinkActuate@ attribute.
xlinkActuate :: AttrTag
xlinkActuate = makeAttribute "xlink:actuate"

-- | The @xlinkArcrole@ attribute.
xlinkArcrole :: AttrTag
xlinkArcrole = makeAttribute "xlink:arcrole"

-- | The @xlinkHref@ attribute.
xlinkHref :: AttrTag
xlinkHref = makeAttribute "xlink:href"

-- | The @xlinkRole@ attribute.
xlinkRole :: AttrTag
xlinkRole = makeAttribute "xlink:role"

-- | The @xlinkShow@ attribute.
xlinkShow :: AttrTag
xlinkShow = makeAttribute "xlink:show"

-- | The @xlinkTitle@ attribute.
xlinkTitle :: AttrTag
xlinkTitle = makeAttribute "xlink:title"

-- | The @xlinkType@ attribute.
xlinkType :: AttrTag
xlinkType = makeAttribute "xlink:type"

-- | The @xmlBase@ attribute.
xmlBase :: AttrTag
xmlBase = makeAttribute "xml:base"

-- | The @xmlLang@ attribute.
xmlLang :: AttrTag
xmlLang = makeAttribute "xml:lang"

-- | The @xmlSpace@ attribute.
xmlSpace :: AttrTag
xmlSpace = makeAttribute "xml:space"

-- | The @y@ attribute.
y :: AttrTag
y = makeAttribute "y"

-- | The @y1@ attribute.
y1 :: AttrTag
y1 = makeAttribute "y1"

-- | The @y2@ attribute.
y2 :: AttrTag
y2 = makeAttribute "y2"

-- | The @ychannelselector@ attribute.
yChannelselector :: AttrTag
yChannelselector = makeAttribute "yChannelSelector"

-- | The @z@ attribute.
z :: AttrTag
z = makeAttribute "z"

-- | The @zoomandpan@ attribute.
zoomAndPan :: AttrTag
zoomAndPan = makeAttribute "zoomAndPan"

-- | SVG Elements
text :: Element
text = makeSvgElem "text"

defs :: Element
defs = makeSvgElem "defs"

radialGradient :: Element
radialGradient = makeSvgElem "radialGradient"

path_ :: Element
path_ = makeSvgElem "path"

g :: Element
g = makeSvgElem "g"

a :: Element
a = makeSvgElem "a"

svg11 :: Element
svg11 = makeSvgElem "svg"
  & attributes . at "xmlns:xlink" ?~ "http://www.w3.org/1999/xlink"
  & attributes . at "version" ?~ "1.1"