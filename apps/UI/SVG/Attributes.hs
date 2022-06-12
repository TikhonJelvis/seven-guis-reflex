module UI.SVG.Attributes where

import           Data.Hashable (Hashable)
import           Data.Map      (Map)
import           Data.String   (IsString (..))
import           Data.Text     (Text)

import           GHC.Generics  (Generic)

import           UI.Attributes (AsAttributeValue (..), Attribute (..),
                                Lowercase (..))
import           UI.Color      (Color)
import           UI.SVG.Path   (Path)

-- * Sizes and Positions

-- | The x coordinate of an element in the user coordinate system.
x :: Attribute '["image", "rect", "svg", "use"] Double
x = Attribute "x"

-- | The y coordinate of an element in the user coordinate system.
y :: Attribute '["image", "rect", "svg", "use"] Double
y = Attribute "y"

-- | The x coordinate of the start point in a line.
x1 :: Attribute '["line", "linearGradient"] Double
x1 = Attribute "x1"

-- | 'x1' but cool
x₁ :: Attribute '["line", "linearGradient"] Double
x₁ = x1

-- | The x coordinate of the end point in a line.
x2 :: Attribute '["line", "linearGradient"] Double
x2 = Attribute "x2"

-- | 'x2' but cool
x₂ :: Attribute '["line", "linearGradient"] Double
x₂ = x2

-- | The x coordinate of the start point in a line.
y1 :: Attribute '["line", "linearGradient"] Double
y1 = Attribute "y1"

-- | 'y1' but cool
y₁ :: Attribute '["line", "linearGradient"] Double
y₁ = y1

-- | The x coordinate of the start point in a line.
y2 :: Attribute '["line", "linearGradient"] Double
y2 = Attribute "y2"

-- | 'y2' but cool
y₂ :: Attribute '["line", "linearGradient"] Double
y₂ = y2

-- | The horizontal length of an element in the user coordinate system.
width :: Attribute '["image", "mask", "pattern", "rect", "svg", "use"] Double
width = Attribute "width"

-- | The vertical length of an element in the user coordiante system.
height :: Attribute '["image", "mask", "pattern", "rect", "svg", "use"] Double
height = Attribute "height"

-- | The x coordinate of the shape's center.
cx :: Attribute '["circle", "ellipse", "radialGradient"] Double
cx = Attribute "cx"

-- | The y coordinate of the shape's center.
cy :: Attribute '["circle", "ellipse", "radialGradient"] Double
cy = Attribute "cy"

-- | The radius of the shape.
r :: Attribute '["circle", "radialGradient"] Double
r = Attribute "r"

-- * Paths

-- | The @d@ attribute that specifies the shape of a path.
--
-- See 'Path' for details and examples.
d :: Attribute '["path", "glyph", "missing-glyph"] Path 
d = Attribute "d"

-- | The total length of a path in user units. Setting this scales the
-- distance for drawing an element by @pathLength/computedLength)@.
pathLength :: Attribute '["circle", "ellipse", "pathLength", "line", "path", "polygon", "polyline", "rect"] Double
pathLength = Attribute "pathLength"

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
spreadMethod = Attribute "spreadMethod"

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
gradientUnits = Attribute "gradientUnits"

-- ** Gradient Stops

-- | The color for a gradient stop.
--
-- See 'linear' and 'radial' for details.
stop_color :: Attribute '["stop"] Color
stop_color = Attribute "stop-color"

-- | How far along the gradient to place this stop.
offset :: Attribute '["stop"] Double
offset = Attribute "offset"

-- * Presentation

-- ** Fill

-- | Set the @fill@ property of an element.
fill :: Paint -> Map Text Text
fill paint = [("fill", toAttributeValue paint)]

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
