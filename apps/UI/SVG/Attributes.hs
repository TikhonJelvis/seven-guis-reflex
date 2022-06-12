{-# OPTIONS_GHC -Wno-name-shadowing #-}
module UI.SVG.Attributes where

import           Data.Hashable                (Hashable)
import           Data.Map                     (Map)
import           Data.Maybe                   (listToMaybe)
import           Data.String                  (IsString (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import           GHC.Generics                 (Generic)

import           Linear                       (V2 (..))

import           Text.ParserCombinators.ReadP (readP_to_S, readS_to_P)
import qualified Text.Printf                  as Text

import           UI.Attributes                (AsAttributeValue (..),
                                               Attribute (..), Lowercase (..),
                                               Transform (..), native,
                                               skipHtmlWhitespace)
import           UI.Color                     (Color)
import           UI.Style                     (Angle, px, toCss)
import           UI.SVG.Path                  (Path)

-- * Sizes and Positions

-- | The x coordinate of an element in the user coordinate system.
x :: Attribute '["image", "rect", "svg", "use"] Double
x = native "x"

-- | The y coordinate of an element in the user coordinate system.
y :: Attribute '["image", "rect", "svg", "use"] Double
y = native "y"

-- | The x coordinate of the start point in a line.
x1 :: Attribute '["line", "linearGradient"] Double
x1 = native "x1"

-- | 'x1' but cool
x₁ :: Attribute '["line", "linearGradient"] Double
x₁ = x1

-- | The x coordinate of the end point in a line.
x2 :: Attribute '["line", "linearGradient"] Double
x2 = native "x2"

-- | 'x2' but cool
x₂ :: Attribute '["line", "linearGradient"] Double
x₂ = x2

-- | The x coordinate of the start point in a line.
y1 :: Attribute '["line", "linearGradient"] Double
y1 = native "y1"

-- | 'y1' but cool
y₁ :: Attribute '["line", "linearGradient"] Double
y₁ = y1

-- | The x coordinate of the start point in a line.
y2 :: Attribute '["line", "linearGradient"] Double
y2 = native "y2"

-- | 'y2' but cool
y₂ :: Attribute '["line", "linearGradient"] Double
y₂ = y2

-- | The horizontal length of an element in the user coordinate system.
width :: Attribute '["image", "mask", "pattern", "rect", "svg", "use"] Double
width = native "width"

-- | The vertical length of an element in the user coordiante system.
height :: Attribute '["image", "mask", "pattern", "rect", "svg", "use"] Double
height = native "height"

-- | The x coordinate of the shape's center.
cx :: Attribute '["circle", "ellipse", "radialGradient"] Double
cx = native "cx"

-- | The y coordinate of the shape's center.
cy :: Attribute '["circle", "ellipse", "radialGradient"] Double
cy = native "cy"

-- | The radius of the shape.
r :: Attribute '["circle", "radialGradient"] Double
r = native "r"

-- ** Transforms

    -- TODO: better way to share between SVG attributes and CSS
    -- properties?
-- | A list of CSS transform functions (see 'Transform') to apply to
-- the element and its children.
--
-- When the @transform@ attribute is set multiple times, it will be
-- combined with functions added later appearing later in the list of
-- transforms.
--
-- __Example__
--
-- Rotate a rectangle by 45°:
--
-- @
-- rect [ height    =: 10
--      , width     =: 20
--      , fill      =: "#36f"
--      , transform =: [Rotate (Deg 45)]
--      ]
-- @
--
-- Order matters. Translate /then/ rotate a rectangle:
--
-- @
-- rect [ height    =: 10
--      , width     =: 20
--      , fill      =: "#36f"
--      , transform =: [translate (V2 10 10), rotate (Deg 45)]
--      ]
-- @
--
-- Compare with rotating /then/ translating:
--
-- @
-- rect [ height    =: 10
--      , width     =: 20
--      , fill      =: "#36f"
--      , transform =: [rotate (Deg 45), translate (V2 10 10)]
--      ]
-- @
--
-- Multiple transforms are combined with later transform functions
-- added after earlier ones. The same as the previous example:
--
-- @
-- rect [ height    =: 10
--      , width     =: 20
--      , fill      =: "#36f"
--      , transform =: [rotate (Deg 45)]
--      , transform =: [translate (V2 10 10)]
--      ]
-- @
transform :: Attribute '["SVG"] [Transform]
transform = native "transform"

-- | Translate an element along the given X and Y distances in @px@.
--
-- __Example__
--
-- Move a rectangle down and to the left (or right in RTL mode):
--
-- @
-- rect [ height    =: 10
--      , width     =: 20
--      , fill      =: #36f
--      , transform =: [translate (V2 10 20)]
--      ]
-- @
translate :: V2 Double -> Transform
translate (V2 x y) = Translate (px x) (px y) "0"

-- | Uniformly scale the element.
--
-- __Example__
--
-- Scale a group of elements:
--
-- @
-- g [ transform =: [scale 10] ] do
--   circle [ cx =: 10, cy =: 10, r =: 2, fill =: "#36f" ]
--   circle [ cx =: 15, cy =: 5, r =: 2, fill =: "#f63" ]
-- @
scale :: Double -> Transform
scale n = Scale (toCss n) (toCss n)

-- | Rotate an element in 2D around its @transform-origin@.
--
-- See MDN:
--  * [rotate](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/rotate)
--  * [transform-origin](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin)
rotate :: Angle -> Transform
rotate = Rotate

-- ** viewBox

    -- TODO: better documentation—but I need to understand the details
    -- myself first!
-- | The @viewBox@ attribute defines how the coordinate space of an
-- SVG element maps to the element's viewport.
data ViewBox = ViewBox
  { view_min  :: V2 Double
  , view_size :: V2 Double
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | >>> toAttributeValue $ ViewBox (V2 0 1) (V2 10 20)
-- "0.0 1.0 10.0 20.0"
--
-- >>> fromAttributeValue @ViewBox "\t0.0 1.0 10.0 20.0"
-- Just (ViewBox {view_min = V2 0.0 1.0, view_size = V2 10.0 20.0})
instance AsAttributeValue ViewBox where
  toAttributeValue ViewBox { view_min = V2 min_x min_y, view_size = V2 w h } =
    Text.pack $ Text.printf "%f %f %f %f" min_x min_y w h

  fromAttributeValue = run do
    skipHtmlWhitespace
    view_min  <- V2 <$> double <*> double
    view_size <- V2 <$> double <*> double
    pure ViewBox { view_min, view_size }
    where double = readS_to_P reads
          run p (Text.unpack -> s) = fst <$> listToMaybe (readP_to_S p s)

-- | The @viewBox@ attribute defines how the coordinate space of an
-- SVG element maps to the element's viewport.
viewBox :: Attribute ["marker", "pattern", "svg", "symbol", "view"] ViewBox
viewBox = native "viewBox"

-- * Paths

-- | The @d@ attribute that specifies the shape of a path.
--
-- See 'Path' for details and examples.
d :: Attribute '["path", "glyph", "missing-glyph"] Path
d = native "d"

-- | The total length of a path in user units. Setting this scales the
-- distance for drawing an element by @pathLength/computedLength)@.
pathLength :: Attribute '["circle", "ellipse", "pathLength", "line", "path", "polygon", "polyline", "rect"] Double
pathLength = native "pathLength"

-- * Gradients

-- | SVG has two kind of gradient elements: linear gradients and
-- radial gradients.
type Gradients = '["linearGradient", "radialGradient"]

-- | How a gradient should behave outside of its bounds—what to do
-- when @offset@ would logically be below 0 or above 1?
--
-- >>> toAttributeValue Reflect
-- "reflect"
data SpreadMethod = Pad
                  -- ^ Fill out the remainder of the space with a solid
                  -- color from @offset = 0@ or @offset = 1@ as
                  -- appropriate.

                  | Reflect
                  -- ^ Render the gradient /inverted/ (ie treat the offset
                  -- as @1 - offset@).

                  | Repeat
                  -- ^ Repeat the gradient, resetting @offset@ to @0..1@.
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)
  deriving AsAttributeValue via Lowercase SpreadMethod

-- | How a gradient should behave outside of its bounds.
--
-- See 'SpreadMethod'.
spreadMethod :: Attribute Gradients SpreadMethod
spreadMethod = native "spreadMethod"

-- | The coordinate system for a gradient.
--
-- This defines how gradient coordinates are translated to actual
-- coordinates when the gradient is used.
--
-- >>> toAttributeValue UserSpaceOnUse
-- "userSpaceOnUse"
data GradientUnits = UserSpaceOnUse
                     -- ^ Use the __user coordinates__ at the time and
                     -- place the gradient is applied.
                     --
                     -- Percentage values are computed relative to the
                     -- /current SVG viewport/.

                   | ObjectBoundingBox
                     -- ^ Use coordinates defined based on the
                     -- /bounding box/ of the element the gradient
                     -- applies to.
                     --
                     -- Percentage values are computed relative to the
                     -- bounding box.
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance AsAttributeValue GradientUnits where
  toAttributeValue = \case
    UserSpaceOnUse    -> "userSpaceOnUse"
    ObjectBoundingBox -> "objectBoundingBox"

  fromAttributeValue = \case
    "userSpaceOnUse"    -> Just UserSpaceOnUse
    "objectBoundingBox" -> Just ObjectBoundingBox
    _                   -> Nothing

-- | The coordinate system for the gradient.
--
-- See 'GradientUnits'
gradientUnits :: Attribute Gradients GradientUnits
gradientUnits = native "gradientUnits"

-- ** Gradient Stops

-- | The color for a gradient stop.
--
-- See 'linear' and 'radial' for details.
stop_color :: Attribute '["stop"] Color
stop_color = native "stop-color"

-- | How far along the gradient to place this stop.
offset :: Attribute '["stop"] Double
offset = native "offset"

-- * Presentation

-- ** Fill

-- | Determines how to paint the interior element.
--
-- __Examples__
--
-- Solid color:
--
-- @
-- [ fill =: "#36f"]
-- @
--
-- Paint with a pre-defined gradient:
--
-- @
-- [ fill =: paintWith "my-gradient" ]
-- @
fill :: Attribute
  ["altGlyph", "circle", "ellipse", "path", "polygon", "polyline", "rect",
   "text", "textPath", "tref", "tspan"]
  Paint
fill = native "fill"

             -- TODO: illustrated examples
-- | The algorithm for filling complex shapes.
--
-- To determine whether a point is "inside" a complex path, we draw a
-- ray from that point and look at how often it crosses the path
-- left-to-right (@ltr@) and right-to-left (@rtl@). Then we apply the
-- 'FillRule' to /the difference between the two/ (@ltr - rtl@) to
-- determine whether the point counts as "inside" the path.
--
-- Note: this means that the order of points you specify in a path can
-- affect which points count as "inside" and "outside", even if the
-- path defines the same shape.
--
-- See MDN:
-- [fill-rule](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-rule)
data FillRule = Nonzero
              -- ^ A point counts as "inside" if @ltr - rtl ≠ 0@.
              | Evenodd
              -- ^ A point counts as "inside" if @ltr - rtl@ is odd
              -- and outside if @ltr - rtl@ is even.
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)
  deriving AsAttributeValue via Lowercase FillRule

-- ** Stroke

-- | Set just the @stroke@ attribute.
stroke :: Paint -> Map Text Text
stroke paint = [("stroke", toAttributeValue paint)]

-- | The shape at the end of lines.
data Linecap = Butt
             -- ^ Lines are closed with a straight edge perpendicular
             -- to the direction of the line.
             | Square
             -- ^ Same as 'Butt' but extends the stroke slightly
             -- beyond its actual path.
             | Round
             -- ^ The ends of a line are rounded, with the radius
             -- determined by @stroke-width@.
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance AsAttributeValue Linecap where
  toAttributeValue = \case
    Butt   -> "butt"
    Square -> "square"
    Round  -> "round"

  fromAttributeValue = \case
    "butt"   -> Just Butt
    "square" -> Just Square
    "round"  -> Just Round
    _        -> Nothing

-- | How to draw the joints between two line segments.
data Linejoin = Miter | Round_ | Bevel
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance AsAttributeValue Linejoin where
  toAttributeValue = \case
    Miter  -> "miter"
    Round_ -> "round"
    Bevel  -> "bevel"

  fromAttributeValue = \case
    "miter" -> Just Miter
    "round" -> Just Round_
    "bevel" -> Just Bevel
    _       -> Nothing

-- ** Paint

-- | How to render the @fill@ or @stroke@ of an element.
data Paint = None
           -- ^ Do not render the fill or stroke

           | Color Color
           -- ^ A solid color

             -- TODO: structured URL type?
           | Url Text (Maybe Color)
           -- ^ A URL referencing a "paint server": a
           -- @linearGradient@, @pattern@ or @radialGradient@

           | ContextFill
           -- ^ Use the @fill@ from a __context element__

           | ContextStroke
           -- ^ Use the @stroke@ from a __context element__
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance AsAttributeValue Paint where
  toAttributeValue = \case
    None                  -> "none"
    Color c               -> toAttributeValue c
    Url u Nothing         -> url u
    Url u (Just fallback) -> url u <> " " <> toAttributeValue fallback
    ContextFill           -> "context-fill"
    ContextStroke         -> "context-stroke"
    where url u = "url(" <> u <> ")"

  fromAttributeValue = error "parsePaint not implemented yet"

    -- TODO: implement parsePaint!

-- | @"#fff"@/etc for colors, @"none"@ for 'None', @"context-fill"@ and @"context-stroke"@
--
-- use 'Url' constructor for URLs
instance IsString Paint where
  fromString = \case
    "none"           -> None
    "context-fill"   -> ContextFill
    "context-stroke" -> ContextStroke
    c                -> Color $ fromString c

-- | A paint referencing a /paint server/ with the given id and no
-- fallback.
--
-- >>> paintWith "myCircle"
-- Url "#myCircle" Nothing
--
-- Works well with 'defs':
--
-- @
-- do
--   let c = circle (pure (Circle 0 10)) (pure $ fill "#fff")
--   defs [("bg", \ attrs -> pattern_ attrs c)]
--   rect (pure (Rectangle 100 100 0 0)) (pure $ fill $ paintWith "bg")
-- @
paintWith :: Text -> Paint
paintWith id_ = Url ("#" <> id_) Nothing
