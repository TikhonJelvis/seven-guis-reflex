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

  , module UI.SVG.Attributes

  , circle
  , Circle (..)

  , rect
  , Rectangle (..)

  , module UI.SVG.Path
  , path

  , svgAttribute
  , svgAttributes
  )
where

import           Control.Applicative (liftA2)

import           Data.Foldable       (sequenceA_)
import           Data.Hashable       (Hashable (..))
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Data.Text.Display   (Display (..))

import           GHC.Generics        (Generic)

import           Reflex              (Dynamic, Reflex)
import qualified Reflex
import qualified Reflex.Dom          as Dom

import           UI.Attributes       (RelativeLength, ToAttributeValue (..),
                                      ToAttributes (..), with, setProperty)
import           UI.Element          (Dom, elDynAttrNs')
import qualified UI.Event            as Event
import           UI.IsElement        (FromElement (..), IsElement (..))
import           UI.Point
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
  -- hack to work around what seems to be a WebKitGTK bug
  --
  -- without this, % in transforms and transform-origin settings don't
  -- work when the page loads!
  done <- Reflex.delay 0.01 =<< Dom.getPostBuild
  after <- Reflex.delay 0.01 done
  forceTransformBox <- Reflex.holdDyn id $
    Reflex.leftmost [setTransformBox <$ done, id <$ after]
  let attributes' = Reflex.zipDynWith ($) forceTransformBox attributes

  elDynAttrNs' (Just svgNamespace) tag attributes' body
  where setTransformBox = setProperty "transform-box" ""
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

-- ** Shapes

-- | A circle.
data Circle = Circle
  { center :: !Point
  -- ^ The (x, y) coordinates for the circle's center.
  , radius :: !Double
  -- ^ The circle's radius.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance ToAttributes Circle where
  toAttributes Circle { center = Point cx cy, radius = r } =
    [ ("cx", toAttributeValue cx)
    , ("cy", toAttributeValue cy)
    , ("r", toAttributeValue r)
    ]

instance Display Circle where
  displayBuilder Circle { center, radius } =
    "Circle at " <> displayBuilder center <>
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
  { height :: RelativeLength
  , width  :: RelativeLength
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

rect :: forall m t. Dom t m
     => Dynamic t Rectangle
     -- ^ Position and dimensions
     -> Dynamic t (Map Text Text)
     -- ^ Additional attributes. The 'Rectangle' argument will
     -- override @x@, @y@, @width@ and @height@ here.
     -> m (Svg t)
rect rectangle attributes =
  fst <$> svgDynAttr' "rect" (liftA2 with rectangle attributes) (pure ())

-- *** Paths

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
