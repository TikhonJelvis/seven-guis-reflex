module UI.SVG
  ( Svg

  , svg

  , g
  , g_

  , defs
  , Def
  , def

  , use
  , pattern_

  , module UI.SVG.Attributes

  , circle
  , Circle (..)

  , rect
  , Rectangle (..)

  , module UI.SVG.Path
  , path

  , Stop (..)
  , linear
  , linearPath
  , radial
  , radialPath
  , Spread (..)
  , GradientUnits (..)

  , svgAttribute
  , svgAttributes
  )
where

import           Control.Applicative (liftA2)
import           Control.Monad       (forM)

import           Data.Foldable       (sequenceA_)
import           Data.Functor        ((<&>))
import           Data.Hashable       (Hashable (..))
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Proxy          (Proxy (..))
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           GHC.Generics        (Generic)
import           GHC.TypeLits        (KnownSymbol, symbolVal)

import           Linear              (V2(..), _x, _y)

import           Reflex              (Dynamic, Reflex)
import qualified Reflex.Dom          as Dom

import           UI.Attributes       (AsAttributeValue (..), Attribute (..),
                                      AttributeSet, AttributeValue,
                                      Lowercase (..), href, id_, toDom, (=:),
                                      (==:))
import           UI.Color            (Color)
import           UI.Element          (Dom, createElement)
import qualified UI.Event            as Event
import           UI.Id               (Id (..))
import           UI.IsElement        (FromElement (..), IsElement (..))
import           UI.SVG.Attributes
import           UI.SVG.Path
import qualified UI.Url              as Url
import Control.Lens (view)

-- * SVG Elements

newtype Svg t = Svg (Dom.Element Event.EventResult Dom.GhcjsDomSpace t)

instance FromElement Svg where
  type EventResult Svg = Event.EventResult
  fromElement = Svg

instance IsElement (Svg t) where
  rawElement (Svg e) = Dom._element_raw e

instance Reflex t => Dom.HasDomEvent t (Svg t) en where
  type DomEventType (Svg t) en = Event.EventResultType en
  domEvent eventName (Svg e) = Dom.domEvent eventName e

-- ** Creating Any Element

-- | Create an SVG element.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- html @"circle" [class_ =: "ball"] (pure ())
-- @
svg :: forall element a m t. (KnownSymbol element, Dom t m)
    => AttributeSet t element "SVG"
    -- ^ attributes
    -> m a
    -- ^ body
    -> m (Svg t, a)
svg = createElement Nothing tag . toDom
  where tag = Text.pack $ symbolVal (Proxy :: Proxy element)
{-# INLINABLE svg #-}

-- ** Grouping

-- | A group of SVG elements.
g :: forall a m t. Dom t m
  => AttributeSet t "g" "SVG"
  -- ^ Attributes
  -> [m a]
  -- ^ SVG elements to group
  -> m (Svg t, [a])
g attributes = svg attributes . sequenceA

-- | A group of SVG elements, discarding the value returned from
-- creating each element.
g_ :: forall a m t. Dom t m
   => AttributeSet t "g" "SVG"
   -- ^ Attributes
   -> [m a]
   -- ^ SVG elements to group
   -> m (Svg t)
g_ attributes = fmap fst . svg attributes . sequenceA_

-- | Define elements that are not rendered immediately, but can be
-- used in other parts of the SVG (ie with 'use').
--
-- __Examples__
--
-- Define and use an element:
--
-- @
-- do let c = Circle { center = V2 0 0, radius = 5 }
--    defs [ def "myElement" (circle (pure c)) [stroke =: "none"] ]
--    use "myElement" (pure [("x", "10"), ("y", "4")])
-- @
--
-- Define a pattern and use it to fill an element:
--
-- @
-- do let c = Circle { center = V2 0 0, radius = 10 }
--    defs [ def "bg" (circle $ pure c) [fill =: "#fff"] ]
--    rect (pure (Rectangle 100 100 0 0)) [fill =: paintWith "bg"]
-- @
defs :: forall a m t. Dom t m
     => [Def t m a]
     -- ^ A list of definitions.
     -> m (Map Id a)
defs definitions =
  snd <$> svg [] do
    Map.fromList <$> forM definitions \ (Def name create base) -> do
      result <- create $ base <> [id_ =: name]
      pure (name, result)

-- | A definition combining an 'Id', a way to create elements and a
-- base set of attributes.
--
-- When this is used, the @id@ attribute ('id_') in the base
-- attributes will be overridden if set.
data Def t m a where
  Def :: Id
      -> (AttributeSet t element "SVG" -> m a)
      -> AttributeSet t element "SVG"
      -> Def t m a

-- | Define an element with the given id and attributes.
--
-- If the @id@ attribute is set in the base attributes, it will be
-- overridden.
--
-- @
-- defs
--   [ def "example-circle" (circle (pure c)) [stroke =: "#000"]
--   , def "example-gradient" linearGradient gradientAttrs
--   ]
-- @
def :: forall element a m t. Dom t m
    => Id
    -- ^ id to define
    -> (AttributeSet t element "SVG" -> m a)
    -- ^ function to create element given an attribute set
    -> AttributeSet t element "SVG"
    -- ^ base attribute set to pass into function
    -> Def t m a
def = Def

-- | Use an element defined elsewhere in the current document,
-- referenced by id.
--
-- __Examples__
--
-- Define and use an element:
--
-- @
-- do let c = Circle { center = V2 0 0, radius = 5 }
--    defs [ def "myElement" (circle (pure c)) [] ]
--    use "myElement" [ x =: 10, y =: 4 ]
-- @
use :: forall m t. Dom t m
    => Id
    -- ^ The id of the element to use
    -> AttributeSet t "use" "SVG"
    -- ^ Other attributes of the @use@ tag. An @href@ set here will be
    -- overridden.
    --
    -- Note that /most/ attributes will be the same as the element
    -- being used—only @x@, @y@, @width@ and @height@ will have any
    -- effect, but other attributes are still allowed.
    --
    -- See MDN:
    -- [use](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use)
    -> m (Svg t)
use elementId attributes =
  fst <$> svg (attributes <> [ href =: Url.byId elementId ]) (pure ())

-- | A @pattern@ element defines an object that can be tiled along x-
-- and y-coordinate intervals.
--
-- __Example__
--
-- A rectangle tiled with white circles:
--
-- @
-- do
--   let c = circle (pure (Circle 0 10)) (pure $ fill "#fff")
--   defs [("bg", \ attrs -> pattern_ attrs c)]
--   rect (pure (Rectangle 100 100 0 0)) (pure $ fill $ paintWith "bg")
-- @
pattern_ :: forall a m t. Dom t m
         => AttributeSet t "pattern" "SVG"
         -- ^ Attributes
         -> m a
         -- ^ Pattern contents
         -> m (Svg t, a)
pattern_ = svg

-- ** Shapes

-- | A circle with a given center and radius.
data Circle = Circle
  { center :: !(V2 Double)
  -- ^ The (x, y) coordinates for the circle's center.
  , radius :: !Double
  -- ^ The circle's radius.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | Create a circle element with the given settings.
--
-- __Example__
--
-- A circle with a static center and radius:
--
-- @
-- let c = Circle { center = (50, 50), radius = 50}
-- in
-- circle (pure c) [stroke_width =: 4, stroke =: "#3366ff"]
-- @
circle :: forall m t. Dom t m
       => Dynamic t Circle
       -- ^ The circle shape itself
       -> AttributeSet t "circle" "SVG"
       -- ^ Additional attributes. The 'Circle' argument will override
       -- @cx@, @cy@ and @r@.
       -> m (Svg t)
circle shape attributes = fst <$> svg (attributes <> override) (pure ())
  where override = [ cx ==: view _x . center <$> shape
                   , cy ==: view _y . center <$> shape
                   ,  r ==: radius <$> shape
                   ]

-- | The x coordinate of the shape's center.
cx :: Attribute "cx" '["circle", "ellipse", "radialGradient"]
cx = Attribute

type instance AttributeValue "cx" '["circle", "ellipse", "radialGradient"] = Double

-- | The y coordinate of the shape's center.
cy :: Attribute "cy" '["circle", "ellipse", "radialGradient"]
cy = Attribute


type instance AttributeValue "cy" '["circle", "ellipse", "radialGradient"] = Double

-- | The radius of the shape.
r :: Attribute "r" '["circle", "radialGradient"]
r = Attribute

type instance AttributeValue "r" '["circle", "radialGradient"] = Double


-- | A rectangle specified as a point along with a width and a height.
data Rectangle = Rectangle
  { position :: !(V2 Double)
  , width    :: !Double
  , height   :: !Double
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | Create a retangle (@rect@ element).
rect :: forall m t. Dom t m
     => Dynamic t Rectangle
     -- ^ Position and dimensions
     -> Dynamic t (Map Text Text)
     -- ^ Additional attributes. The 'Rectangle' argument will
     -- override @x@, @y@, @width@ and @height@ here.
     -> m (Svg t)
rect rectangle attributes =
  fst <$> svgDynAttr' "rect" (liftA2 with rectangle attributes) (pure ())

-- | The @path@ element with a dynamic @d@ attribute.
path :: forall m t. Dom t m
     => Dynamic t Path
     -- ^ @d@ attribute for the @path@ which specifies the geometry of
     -- the path itself.
     -> Dynamic t (Map Text Text)
     -- ^ Other attributes of the @path@. A value for the @d@
     -- attribute here will be overwritten.
     -> m (Svg t)
path d attributes =
  fst <$> svgDynAttr' "path" (liftA2 with d attributes) (pure ())

-- ** Gradients

-- $ SVG supports __linear__ and __radial__ gradients.
--
-- Gradients are specified as a series of __stops__ which define the
-- colors to transition between along the length of the gradient.
--
-- A gradient is not rendered by itself; it needs to be used as the
-- @stroke@ or @fill@ property of another element. The best way to do
-- this is by defining gradients in a 'defs' block and using
-- 'paintWith':
--
-- @
-- do
--   let stops = [Stop 0 "#fff", Stop 0.5 "#000", Stop 1 "#fff"]
--
--   defs
--     [ ("myLinear", linear (pure stops))
--     , ("myRadial", radial (pure stops))
--     ]
--
--  let sample x y attributes = rect (pure $ r x y) (pure attributes)
--      r x y = Rectangle
--        { height = px 100, width = px 100, x = px x, y = px y }
--  sample 0 0 (fill $ paintWith "myLinear")
--  sample 110 110 (fill $ paintWith "myRadial")
-- @

-- | A gradient is made up of multiple __stops__ that specify the
-- color to transition part of the way through the gradient.
data Stop = Stop
  { offset :: Double
    -- ^ A number between 0 and 1 specifying how far along the
    -- gradient to position the stop.
  , color  :: Color
    -- ^ The color for this stop.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A linear gradient, transitioning between stops along a straight
-- line.
--
-- __Examples__
--
--
linear :: forall m t. Dom t m
       => Dynamic t [Stop]
       -- ^ Gradient stops
       -> Dynamic t (Map Text Text)
       -- ^ Attributes
       -> m (Svg t)
linear stops attributes =
  fst <$> svgDynAttr' "linearGradient" attributes (dynStops stops)

-- | The start and end points of the line that a linear gradient
-- follows.
linearPath :: V2 Double
           -- ^ point corresponding to @offset = 0@
           -> V2 Double
           -- ^ point corresponding to @offset = 1@
           -> Map Text Text
linearPath (V2 x₁ y₁) (V2 x₂ y₂) =
  [ ("x1", toAttributeValue x₁), ("y1", toAttributeValue y₁)
  , ("x2", toAttributeValue x₂), ("y2", toAttributeValue y₂)
  ]

-- | A radial gradient which transitions colors from a center point
-- (@offset = 0@) out in a circle up to some radius (@offset = 1@).
radial :: forall m t. Dom t m
       => Dynamic t [Stop]
       -- ^ Gradient stops
       -> Dynamic t (Map Text Text)
       -- ^ Attributes
       -> m (Svg t)
radial stops attributes =
  fst <$> svgDynAttr' "radialGradient" attributes (dynStops stops)

  -- TODO: handle changing focal point/etc
-- | The center and radius for the circle covered by a radial
-- gradient.
radialPath :: V2 Double
           -- ^ center of the circle, corresponding to @offset = 0@
           -> Double
           -- ^ radius of the circle; @offset = 1@ at points at least
           -- @radius@ away from @center@
           -> Map Text Text
radialPath (V2 x y) r =
  [("cx", toAttributeValue x), ("cy", toAttributeValue y), ("r", toAttributeValue r)]

-- | A dynamic set of @stop@ elements.
dynStops :: forall m t. Dom t m
         => Dynamic t [Stop]
         -> m ()
dynStops stops = Dom.dyn_ $ stops <&> mapM_ \ stop ->
  svgAttr' "stop" (toAttributes stop) (pure ())

-- | How a gradient should behave outside of its bounds—what to do
-- when @offset@ would logically be below 0 or above 1?
--
-- >>> toAttributes Pad
-- fromList [("spreadMethod","pad")]
data Spread = Pad
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
  deriving AsAttributeValue via Lowercase Spread

-- | The coordinate system for a gradient.
--
-- This defines how gradient coordinates are translated to actual
-- coordinates when the gradient is used.
--
-- >>> toAttributes UserSpaceOnUse
-- fromList [("gradientUnits","userSpaceOnUse")]
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

-- * SVG Namespace

-- | The namespace for SVG elements: @http://www.w3.org/2000/svg@.
svgNamespace :: Text
svgNamespace = "http://www.w3.org/2000/svg"

-- | Wrap a text attribute name into an 'AttributeName' with the SVG
-- namespace (see 'svgNamespace').
svgAttribute :: Text -> Dom.AttributeName
svgAttribute = Dom.AttributeName (Just svgNamespace)

-- | Wrap a text attribute map into a map with 'AttributeName' keys
-- namespaced for SVG (see 'svgNamespace').
svgAttributes :: Map Text a -> Map Dom.AttributeName a
svgAttributes = Map.mapKeys svgAttribute
