module UI.SVG.Attributes where

import qualified Data.Colour        as Colour
import           Data.Default.Class (Default (..))
import           Data.Hashable      (Hashable)
import           Data.Map           (Map)
import           Data.String        (IsString (..))
import           Data.Text          (Text)

import           GHC.Generics       (Generic)

import           UI.Attributes      (AsAttributeValue (..), Lowercase (..))
import           UI.Color           (Color, fromColour)

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

-- | How to draw the lines of a shape.
data Stroke = Stroke
  { color    :: Color
  , width    :: Double
  -- ^ stroke width in px
  , linecap  :: Linecap
  , linejoin :: Linejoin
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance Default Stroke where
  def = Stroke
    { color    = fromColour Colour.black
    , width    = 0
    , linecap  = Butt
    , linejoin = Miter
    }

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
