module UI.SVG
  ( Svg

  , svg
  , svgAttr
  , svgDynAttr

  , svg'
  , svgAttr'
  , svgDynAttr'

  , g
  , g_
  , defs
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
import           Data.Text           (Text)
import           Data.Text.Display   (Display (..))

import           GHC.Generics        (Generic)

import           Linear              (V2)
import           Linear.V2           (V2 (..))

import           Reflex              (Dynamic, Reflex)
import qualified Reflex.Dom          as Dom

import           UI.Attributes       (AsAttribute (..), RelativeLength,
                                      ShowLowercase (..), ToAttributeValue (..),
                                      ToAttributes (..), href, id_, with)
import           UI.Color            (Color)
import           UI.Element          (Dom, elDynAttrNs')
import qualified UI.Event            as Event
import           UI.IsElement        (FromElement (..), IsElement (..))
import           UI.SVG.Attributes
import           UI.SVG.Path

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
svg :: forall a m t. Dom t m
    => Text
    -- ^ Tag
    -> m a
    -- ^ Body
    -> m a
svg tag body = snd <$> svg' tag body
{-# INLINABLE svg #-}

-- | Create an SVG element with static attributes.
svgAttr :: forall a m t. Dom t m
        => Text
        -- ^ Tag name
        -> Map Text Text
        -- ^ Static attributes
        -> m a
        -- ^ Body
        -> m a
svgAttr tag attr body = snd <$> svgAttr' tag attr body
{-# INLINABLE svgAttr #-}

-- | Create an SVG element with a dynamic set of attributes.
svgDynAttr :: forall a m t. Dom t m
           => Text
           -- ^ Tag name
           -> Dynamic t (Map Text Text)
           -- ^ Attributes
           -> m a
           -- ^ Body
           -> m a
svgDynAttr tag attr body = snd <$> svgDynAttr' tag attr body
{-# INLINABLE svgDynAttr #-}

-- *** With Element Results

-- | Create and return an SVG element.
svg' :: forall a m t. Dom t m
     => Text
     -- ^ Tag name
     -> m a
     -- ^ Body
     -> m (Svg t, a)
svg' tag = svgAttr' tag []
{-# INLINABLE svg' #-}

-- | Create and return an SVG element with a static set of attributes.
svgAttr' :: forall a m t. Dom t m
         => Text
         -- ^ Tag name
         -> Map Text Text
         -- ^ Static attributes
         -> m a
         -- ^ Body
         -> m (Svg t, a)
svgAttr' tag = svgDynAttr' tag . pure
{-# INLINABLE svgAttr' #-}

-- | Create and return an SVG element with a dynamic set of attributes.
svgDynAttr' :: forall a m t. Dom t m
            => Text
            -- ^ Tag name
            -> Dynamic t (Map Text Text)
            -- ^ Attributes
            -> m a
            -- ^ Body
            -> m (Svg t, a)
svgDynAttr' tag attributes body = do
  elDynAttrNs' (Just svgNamespace) tag attributes body
{-# INLINABLE svgDynAttr' #-}

-- ** Grouping

-- | A group of SVG elements.
g :: forall a m t. Dom t m
  => Dynamic t (Map Text Text)
  -- ^ Attributes
  -> [m a]
  -- ^ SVG elements to group
  -> m (Svg t, [a])
g attributes = svgDynAttr' "g" attributes . sequenceA

-- | A group of SVG elements, discarding the value returned from
-- creating each element.
g_ :: forall a m t. Dom t m
   => Dynamic t (Map Text Text)
  -- ^ Attributes
  -> [m a]
  -- ^ SVG elements to group
  -> m (Svg t)
g_ attributes = fmap fst . svgDynAttr' "g" attributes . sequenceA_

-- | Define elements that are not rendered immediately, but can be
-- used in other parts of the SVG (ie with 'use').
--
-- __Examples__
--
-- Define and use an element:
--
-- @
-- do
--   defs [("myElement",
--          circle (pure Circle { center = V2 0 0, radius = 5 }))
--        ]
--   use "myElement" (pure [("x", "10"), ("y", "4")])
-- @
--
-- Define a pattern and use it to fill an element:
--
-- @
-- do
--   let c = circle (pure (Circle 0 10)) (pure $ fill "#fff")
--   defs [("bg", \ attrs -> pattern_ attrs c)]
--   rect (pure (Rectangle 100 100 0 0)) (pure $ fill $ paintWith "bg")
-- @
defs :: forall a m t. Dom t m
     => Map Text (Dynamic t (Map Text Text) -> m a)
     -- ^ A map from ids to functions that take attributes and create
     -- elements.
     -> m (Map Text a)
defs (Map.toList -> definitions) =
  snd <$> svg' "defs" do
    Map.fromList <$> forM definitions \ (name, f) -> do
      result <- f (pure $ with (id_ name) [])
      pure (name, result)

-- | Use an element defined elsewhere in the current document,
-- referenced by id.
--
-- __Examples__
--
-- Define and use an element:
--
-- @
-- do
--   defs [("myElement",
--          circle (pure Circle { center = V2 0 0, radius = 5 }) (pure []))
--        ]
--   use "myElement" (pure [("x", "10"), ("y", "4")])
-- @
use :: forall m t. Dom t m
    => Text
    -- ^ The id of the element to use
    -> Dynamic t (Map Text Text)
    -- ^ Other attributes of the @use@ tag. An @href@ set here will be
    -- overridden.
    --
    -- Note that /most/ attributes will be the same as the element
    -- being used—only @x@, @y@, @width@ and @height@ will have any
    -- effect.
    --
    -- See MDN:
    -- [use](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use)
    -> m (Svg t)
use element attributes = fst <$> svgDynAttr' "use"
  (with (href $ "#" <> element) <$> attributes)
  (pure ())

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
         => Dynamic t (Map Text Text)
         -- ^ Attributes
         -> m a
         -- ^ Pattern contents
         -> m (Svg t, a)
pattern_ = svgDynAttr' "pattern"

-- ** Shapes

-- | A circle.
data Circle = Circle
  { center :: !(V2 Double)
  -- ^ The (x, y) coordinates for the circle's center.
  , radius :: !Double
  -- ^ The circle's radius.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance ToAttributes Circle where
  toAttributes Circle { center = V2 cx cy, radius = r } =
    [ ("cx", toAttributeValue cx)
    , ("cy", toAttributeValue cy)
    , ("r", toAttributeValue r)
    ]

instance Display Circle where
  displayBuilder Circle { center = V2 x y, radius } =
    "Circle at (" <> displayBuilder x <> ", " <> displayBuilder y <> ")" <>
    " with radius = " <> displayBuilder radius

-- | Create a circle element with the given settings.
--
-- @
-- let c = Circle { center = (50, 50), radius = 50}
-- in
-- circle c (toAttributes def { width = 4, color = "#36f" })
-- @
circle :: forall m t. Dom t m
       => Dynamic t Circle
       -- ^ Core circle settings.
       -> Dynamic t (Map Text Text)
       -- ^ Additional attributes. The 'Circle' argument will override
       -- @cx@, @cy@ and @r@ in this map.
       -> m (Svg t)
circle circle_ attributes =
  fst <$> svgDynAttr' "circle" (liftA2 with circle_ attributes) (pure ())

-- | A rectangle specified as a point along with a width and a height.
data Rectangle = Rectangle
  { width  :: RelativeLength
  , height :: RelativeLength
  , x      :: RelativeLength
  , y      :: RelativeLength
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance ToAttributes Rectangle where
  toAttributes Rectangle { x, y, width, height } =
    [ ("width", toAttributeValue width)
    , ("height", toAttributeValue height)
    , ("x", toAttributeValue x)
    , ("y", toAttributeValue y)
    ]

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

-- TODO: special handling for stop-opacity?
instance ToAttributes Stop where
  toAttributes Stop { offset, color } =
    [ ("offset", toAttributeValue offset)
    , ("stop-color", toAttributeValue color)
    ]

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
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)
  deriving ToAttributeValue via ShowLowercase Spread
  deriving ToAttributes via AsAttribute "spreadMethod" Spread

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
  deriving ToAttributes via AsAttribute "gradientUnits" GradientUnits

instance ToAttributeValue GradientUnits where
  toAttributeValue = \case
    UserSpaceOnUse    -> "userSpaceOnUse"
    ObjectBoundingBox -> "objectBoundingBox"

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
