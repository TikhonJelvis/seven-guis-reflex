{-# OPTIONS_GHC -Wno-name-shadowing #-}
module UI.Svg.Attributes where

import           Control.Applicative          (optional)

import qualified Data.Char                    as Char
import           Data.Foldable                (toList)
import           Data.Hashable                (Hashable)
import           Data.Maybe                   (listToMaybe)
import           Data.String                  (IsString (..))
import qualified Data.Text                    as Text
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector

import           GHC.Generics                 (Generic)

import           Linear                       (V2 (..))

import           Text.ParserCombinators.ReadP (char, choice, many1, readP_to_S,
                                               readS_to_P, satisfy, sepBy,
                                               string)
import qualified Text.Printf                  as Text

import           UI.Attributes                (AsAttributeValue (..),
                                               Attribute (..),
                                               CombineAttributeValue,
                                               Lowercase (..), native,
                                               skipHtmlWhitespace)
import           UI.Color                     (Color)
import           UI.Id                        (Id)
import           UI.Svg.Path                  (Path)
import           UI.Units                     (Length (..), RelativeLength)
import qualified UI.Url                       as Url

-- * Sizes and Positions

-- | The x coordinate of an element in the user coordinate system.
x :: Attribute RelativeLength
x = native "x"

-- | The y coordinate of an element in the user coordinate system.
y :: Attribute RelativeLength
y = native "y"

-- | The x coordinate of the start point in a line.
x1 :: Attribute RelativeLength
x1 = native "x1"

-- | 'x1' but cool
x₁ :: Attribute RelativeLength
x₁ = x1

-- | The x coordinate of the end point in a line.
x2 :: Attribute RelativeLength
x2 = native "x2"

-- | 'x2' but cool
x₂ :: Attribute RelativeLength
x₂ = x2

-- | The x coordinate of the start point in a line.
y1 :: Attribute RelativeLength
y1 = native "y1"

-- | 'y1' but cool
y₁ :: Attribute RelativeLength
y₁ = y1

-- | The x coordinate of the start point in a line.
y2 :: Attribute RelativeLength
y2 = native "y2"

-- | 'y2' but cool
y₂ :: Attribute RelativeLength
y₂ = y2

-- | The x coordinate of the shape's center.
cx :: Attribute RelativeLength
cx = native "cx"

-- | The y coordinate of the shape's center.
cy :: Attribute RelativeLength
cy = native "cy"

-- | The radius of the shape.
r :: Attribute RelativeLength
r = native "r"

-- | Width as an element-level attribute. For certain SVG elements
-- like @pattern@, this will work but setting @width@ via CSS will
-- not.
width :: Attribute RelativeLength
width = native "width"

-- | Height as an element-level attribute. For certain SVG elements
-- like @pattern@, this will work but setting @height@ via CSS will
-- not.
height :: Attribute RelativeLength
height = native "height"

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

instance CombineAttributeValue ViewBox

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
viewBox :: Attribute ViewBox
viewBox = native "viewBox"

-- * Paths

-- | The @d@ attribute that specifies the shape of a path.
--
-- See 'Path' for details and examples.
d :: Attribute Path
d = native "d"

-- | The total length of a path in user units. Setting this scales the
-- distance for drawing an element by @pathLength/computedLength)@.
pathLength :: Attribute Length
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
  deriving (CombineAttributeValue, AsAttributeValue) via Lowercase SpreadMethod

-- | How a gradient should behave outside of its bounds.
--
-- See 'SpreadMethod'.
spreadMethod :: Attribute SpreadMethod
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

instance CombineAttributeValue GradientUnits
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
gradientUnits :: Attribute GradientUnits
gradientUnits = native "gradientUnits"

-- ** Gradient Stops

-- | The color for a gradient stop.
--
-- See 'linear' and 'radial' for details.
stop_color :: Attribute Color
stop_color = native "stop-color"

-- | How far along the gradient to place this stop.
offset :: Attribute Double
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
fill :: Attribute Paint
fill = native "fill"

             -- TODO: illustrated examples
-- | The algorithm to use for filling complex shapes.
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
  deriving (CombineAttributeValue , AsAttributeValue) via Lowercase FillRule

-- TODO: examples
-- | Set the algorithm to use for filling complex shapes.
--
-- See 'FillRule' for details.
fill_rule :: Attribute FillRule
fill_rule = native "fill-rule"

-- ** Stroke

-- | Elements that support stroke-related attributes:
--
--  * 'stroke'
--  * 'stroke_dasharray'
--  * 'stroke_dashoffset'
--  * 'stroke_linecap'
--  * 'stroke_linejoin'
--  * 'stroke_miterlimit'
--  * 'stroke_opacity'
--  * 'stroke_width'
type Stroked =
  [ "circle"
  , "ellipse"
  , "line"
  , "path"
  , "polygon"
  , "polyline"
  , "rect"
  , "text"
  , "textPath"
  , "tspan"
  ]

-- | How the element's outline is painted.
--
-- __Examples__
--
-- Solid color:
--
-- @
-- [ stroke =: "#36f", stroke_width =: 1 ]
-- @
--
-- Paint with a pre-defined gradient:
--
-- @
-- [ stroke =: paintWith "my-gradient", stroke_width =: 1 ]
-- @
stroke :: Attribute Paint
stroke = native "stroke"

-- | The width of the stroke.
stroke_width :: Attribute Length
stroke_width = native "stroke-width"

-- | Explicitly set the opacity of the stroke.
--
-- Should be a number between 0 and 1.
stroke_opacity :: Attribute Double
stroke_opacity = native "stroke-opacity"

-- | A pattern of lengths specifying the alternating length of dashes
-- and gaps between dashes.
newtype Dasharray = Dasharray (Vector Length)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance CombineAttributeValue Dasharray
instance AsAttributeValue Dasharray where
  toAttributeValue (Dasharray a) = case toList a of
    [] -> "none"
    xs -> Text.intercalate " " $ toAttributeValue <$> xs

  fromAttributeValue = run do
    skipHtmlWhitespace
    xs <- choice
      [ sepBy length (optional (char ',') *> skipHtmlWhitespace)
      , [] <$ string "none"
      ]
    skipHtmlWhitespace
    pure $ Dasharray $ Vector.fromList xs
    where run p (Text.unpack -> s) = fst <$> listToMaybe (readP_to_S p s)
          length = Length . Text.pack <$> many1 (satisfy Char.isAlphaNum) <* skipHtmlWhitespace

-- | A list of lengths specifying the alternating length of dashes and
-- gaps for a dashed stroke.
--
-- __Examples__
--
-- Solid stroke, no dashes:
--
-- @
-- [ stroke_dasharray =: [] ]
-- @
--
-- Regularly-spaced 1px dashes:
--
-- @
-- [ stroke_dasharray =: [1, 1]
-- @
--
-- Longer then shorter dashes with small spaces:
--
-- @
-- [ stroke_dasharray =: [2, 2, 3, 2] ]
-- @
stroke_dasharray :: Attribute Dasharray
stroke_dasharray = native "stroke-dasharray"

                   -- TODO: examples
-- | An offset on rendering the stroke's dasharray.
stroke_dashoffset :: Attribute Length
stroke_dashoffset = native "stroke-dashoffset"

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
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)
  deriving (CombineAttributeValue, AsAttributeValue) via Lowercase Linecap

-- | The shape to draw at the end of lines.
--
-- __Examples__
--
-- @
-- [ stroke_linecap =: Butt ]
-- @
--
-- @
-- [ stroke_linecap =: Square ]
-- @
--
-- @
-- [ stroke_linecap =: Round ]
-- @
stroke_linecap :: Attribute Linecap
stroke_linecap = native "stroke-linecap"

-- | How to draw the joints between two line segments.
data Linejoin = Arcs | Bevel | Miter | MiterClip | Round_
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance CombineAttributeValue Linejoin
instance AsAttributeValue Linejoin where
  toAttributeValue = \case
    Arcs      -> "arcs"
    Bevel     -> "bevel"
    Miter     -> "miter"
    MiterClip -> "miter-clip"
    Round_    -> "round"

  fromAttributeValue = \case
    "arcs"       -> Just Arcs
    "bevel"      -> Just Bevel
    "miter"      -> Just Miter
    "miter-clip" -> Just MiterClip
    "round"      -> Just Round_
    _            -> Nothing


    -- TODO: examples
-- | How to draw the joints between line segments in a stroke.
stroke_linejoin :: Attribute Linejoin
stroke_linejoin = native "stroke-linejoin"

-- | Limit the ratio of the miter length to 'stroke_width'. When the
-- limit is exceeded, the miter becomes a bevel.
--
-- Must be ≥ 1.
stroke_miterlimit :: Attribute Double
stroke_miterlimit = native "stroke-miterlimit"

-- ** Paint

-- | How to render the @fill@ or @stroke@ of an element.
data Paint = None
           -- ^ Do not render the fill or stroke

           | Color Color
           -- ^ A solid color

             -- TODO: structured URL type?
           | Url Url.Url (Maybe Color)
           -- ^ A URL referencing a "paint server": a
           -- @linearGradient@, @pattern@ or @radialGradient@

           | ContextFill
           -- ^ Use the @fill@ from a __context element__

           | ContextStroke
           -- ^ Use the @stroke@ from a __context element__
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance CombineAttributeValue Paint
instance AsAttributeValue Paint where
  toAttributeValue = \case
    None                  -> "none"
    Color c               -> toAttributeValue c
    Url u Nothing         -> url u
    Url u (Just fallback) -> url u <> " " <> toAttributeValue fallback
    ContextFill           -> "context-fill"
    ContextStroke         -> "context-stroke"
    where url u = "url(" <> Url.toText u <> ")"

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
paintWith :: Id -> Paint
paintWith id_ = Url (Url.byId id_) Nothing
