{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module UI.SVG where


import           Control.Applicative    (liftA2)

import           Data.Colour            (AlphaColour, Colour, ColourOps (over))
import qualified Data.Colour            as Colour
import           Data.Colour.RGBSpace   (RGB (..))
import qualified Data.Colour.SRGB       as Colour
import           Data.Default.Class     (Default, def)
import           Data.Hashable          (Hashable (..))
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Display      (Display (..))
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Word              (Word8)

import           GHC.Generics           (Generic)

import qualified Numeric

import           Reflex                 (Dynamic, Reflex)
import qualified Reflex.Dom             as Dom

import           Text.Printf            (printf)

import           UI.Attributes          (ToAttributeValue (..),
                                         ToAttributes (..), with)
import           UI.Element             (Dom, elDynAttrNs')
import qualified UI.Event               as Event
import           UI.IsElement           (FromElement (..), IsElement (..))
import           UI.Point

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


-- ** Arbitrary Elements

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
svgDynAttr' = elDynAttrNs' (Just svgNamespace)
{-# INLINABLE svgDynAttr' #-}

-- ** Presentation

-- *** Color

-- | A CSS color.
newtype Color = Color (AlphaColour Double)
  deriving stock (Eq, Show, Generic)

instance IsString Color where
  fromString ['#', r, g, b] =
    fromString ['#', r, r, g, g, b, b]
  fromString ['#', r, g, b, a] =
    fromString ['#', r, r, g, g, b, b, a, a]
  fromString s@['#', _r1, _r2, _g1, _g2, _b1, _b2] =
    fromColour $ Colour.sRGB24read s
  fromString s@['#', _r1, _r2, _g1, _g2, _b1, _b2, a1, a2] =
    let base = Colour.sRGB24read (take 7 s) in
    Color $ Colour.withOpacity base (toDouble $ readHex [a1, a2])
    where readHex = fst . head . Numeric.readHex
          toDouble x = fromInteger x / 255
  fromString invalid = error $ "Invalid color: " <> invalid

-- | hash based on sRGB + α-channel
instance Hashable Color where
  hashWithSalt s (Color c) = hashWithSalt s (r, g, b, α)
    where α = Colour.alphaChannel c
          RGB r g b = Colour.toSRGB24 (c `over` Colour.black)

-- | Create a 'Color' that is a fully opaque version of the given
-- 'Colour'.
fromColour :: Colour Double -> Color
fromColour = Color . Colour.opaque

-- | Break a 'Color' out into its r, g, b and a channels
-- respectively. Each channel takes on a value 0–255.
toRgba :: Color -> (Word8, Word8, Word8, Word8)
toRgba (Color c) = (r, g, b, a)
  where Colour.RGB r g b = Colour.toSRGB24 (c `over` Colour.black)
        a = round $ 255 * Colour.alphaChannel c

-- | Convert a 'Color' to its CSS representation.
--
-- Colors with 100% opacity are rendered as @#xxxxxx@ and colors with
-- any other level of opacity are rendered as @#xxxxxxxx@.
toCss :: Color -> Text
toCss c = case toRgba c of
  (r, g, b, 0xff) -> Text.pack $ printf "#%.2x%.2x%.2x" r g b
  (r, g, b, a)    -> Text.pack $ printf "#%.2x%.2x%.2x%.2x" r g b a

instance ToAttributeValue Color where
  toAttributeValue = toCss

instance Display Color where
  displayBuilder = Builder.fromText . toCss

-- *** Stroke

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
circle c attrs =
  fst <$> svgDynAttr' "circle" (liftA2 with c attrs) (pure ())

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
