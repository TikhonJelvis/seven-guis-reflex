module UI.SVG.Attributes where

import qualified Data.Colour        as Colour
import           Data.Default.Class (Default (..))
import           Data.Hashable      (Hashable)

import           GHC.Generics       (Generic)

import           UI.Attributes      (ToAttributeValue (..), ToAttributes (..))
import           UI.Color           (Color, fromColour)

-- * Presentation

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

instance ToAttributes Stroke where
  toAttributes Stroke { color, width, linecap, linejoin } =
    [ ("stroke", toAttributeValue color)
    , ("stroke-width", toAttributeValue width)
    , ("linecap", toAttributeValue linecap)
    , ("linejoin", toAttributeValue linejoin)
    ]

-- ** Linecap

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

instance ToAttributeValue Linecap where
  toAttributeValue = \case
    Butt   -> "butt"
    Square -> "square"
    Round  -> "round"

-- | How to draw the joints between two line segments.
data Linejoin = Miter | Round' | Bevel
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance ToAttributeValue Linejoin where
  toAttributeValue = \case
    Miter  -> "miter"
    Round' -> "round"
    Bevel  -> "bevel"
