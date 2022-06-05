{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module UI.SVG where

import           Control.Applicative (liftA2)

import           Data.Bool           (bool)
import qualified Data.Colour         as Colour
import           Data.Default.Class  (Default, def)
import           Data.Hashable       (Hashable (..))
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Display   (Display (..))

import           GHC.Exts            (IsList (..))
import           GHC.Generics        (Generic)

import           Reflex              (Dynamic, Reflex)
import qualified Reflex.Dom          as Dom

import           Text.Printf         (printf)

import           UI.Attributes       (ToAttributeValue (..), ToAttributes (..),
                                      with)
import           UI.Color            (Color, fromColour)
import           UI.Element          (Dom, elDynAttrNs')
import qualified UI.Event            as Event
import           UI.IsElement        (FromElement (..), IsElement (..))
import           UI.Main             (Runnable (..), withCss)
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
circle circle_ attributes =
  fst <$> svgDynAttr' "circle" (liftA2 with circle_ attributes) (pure ())

-- ** Paths

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

-- | A sequence of commands specifying a path.
newtype Path = Path { toCommands :: [Command] }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance IsList Path where
  type Item Path = Command
  fromList = Path
  toList = toCommands

instance ToAttributeValue Path where
  toAttributeValue (Path commands) =
    Text.intercalate " " $ toAttributeValue <$> commands

-- | sets the @d@ attribute
instance ToAttributes Path where
  toAttributes p = [("d", toAttributeValue p)]

-- | Apply a function to each point in a 'Path' (+ @rx@ and @ry@ for
-- elliptic curves).
--
-- Angles and the @large-arc@/@sweep@ flags in elliptic curve commands
-- (@A@ and @a@) are not affected.
mapPath :: (Double -> Double) -> Path -> Path
mapPath f (Path commands) = Path $ map (mapCommand f) commands

-- | Scale a path by a fixed factor.
--
-- This means multiplying every point in the path (+ @rx@ and @ry@ for
-- elliptic curves) by the scaling factor.
scalePath :: Double -> Path -> Path
scalePath α = mapPath (* α)

-- | SVG path commands.
--
-- In SVG, each command comes in an absolute (capitalized) and
-- relative (lowercase) version: for example, move-to has an absolute
-- @M@ and a relative @m@ variant. In this type, the absolute version
-- is 'M' and the relative version is 'M'', with a corresponding
-- function 'm' defined for convenience/readability.
data Command = M !Double !Double
             -- ^ Absolute move.
             --
             -- @M x y@ shifts the /current point/ to @(x, y)@.

             | M' !Double !Double
             -- ^ Relative move.
             --
             -- @M' dx dy@ or @m dx dy@ shifts the /current point/
             -- @(x, y)@ to @(x + dx, y + dy)@.

             | L !Double !Double
             -- ^ Absolute line.
             --
             -- @L x y@ draws a straight line from /current point/ to
             -- @(x, y)@.

             | L' !Double !Double
             -- ^ Relative line.
             --
             -- @L' dx dy@ or @l dx dy@ draws a line from /current
             -- point/ @(x, y)@ to @(x + dx, y + dy)@.

             | H !Double
             -- ^ Absolute horizontal line.
             --
             -- @H x'@ draws a straight line from /current point/ @(x,
             -- y)@ to @(x', y)@.

             | H' !Double
             -- ^ Relative horizontal line.
             --
             -- @H' dx@ or @h dx@ draws a line from /current point/
             -- @(x, y)@ to @(x + dx, y)@.

             | V !Double
             -- ^ Absolute vertical line.
             --
             -- @V y'@ draws a line from /current point/ @(x, y)@ to
             -- @(x, y')@.

             | V' !Double
             -- ^ Relative vertical line.
             --
             -- @V' dy@ or @v dy@ draws a line from /current point/
             -- @(x, y)@ to @(x, y + dy)@.

             | C !Double !Double !Double !Double !Double !Double
             -- ^ Absolute cubic Bézier curve.
             --
             -- @C x₁ y₁ x₂ y₂ x y@ draws a Bézier curve from the
             -- /current point/ to the /end point/ at @(x, y)@, with a
             -- /start control point/ at @(x₁, y₁)@ and an end control
             -- point at @(x₂, y₂)@.

             | C' !Double !Double !Double !Double !Double !Double
             -- ^ Relative cubic Bézier curve.
             --
             -- @C' dx₁ dy₁ dx₂ dy₂ dx dy@ or @c dx₁ dy₁ dx₂ dy₂ dx
             -- dy@ draws a Bézier curve from /current point/ @(x, y)@
             -- to /end point/ @(x + dx, y + dy)@ with a /start
             -- control point/ at @(x + dx₁, y + dy₁)@ and an /end
             -- control point/ at @(x + dx₂, y + dy₂)@.

             | S !Double !Double !Double !Double
             -- ^ Absolute smooth Bézier curve.
             --
             -- @S x₂ y₂ x y@ draws a smooth Bézier curve from
             -- /current point/ to /end point/ @(x, y)@ with an /end
             -- control point/ at @(x₂, y₂)@. The /start control
             -- point/ is either:
             --
             --  * reflection of the /end control point/ of the
             --    previous command if the previous command was a
             --    curve
             --
             --  * /current point/ if the previous command was not a
             --    curve

             | S' !Double !Double !Double !Double
             -- ^ Relative smooth Bézier curve.
             --
             -- @S' dx₂ dy₂ dx dy@ or @s dx₂ dy₂ dx dy@ draws a smooth
             -- Bézier curve from /current point/ @(x, y)@ to /end
             -- point/ @(x + dx, y + dy)@, with an /end control point/
             -- at @(x + dx₂, y + dy₂)@. The /start control point/ is
             -- either:
             --
             --  * reflection of the /end control point/ of the
             --    previous command if the previous command was a
             --    curve
             --
             --  * /current point/ if the previous command was not a
             --    curve

             | Q !Double !Double !Double !Double
             -- ^ Absolute quadratic Bézier curve.
             --
             -- @Q x₁ y₁ x y@ draws a quadratic Bézier curve from
             -- /current point/ to @(x, y)@ with a control point at
             -- @(x₁, y₁)@.

             | Q' !Double !Double !Double !Double
             -- ^ Relative quadratic Bézier curve.
             --
             -- @Q' dx₁ dy₁ dx dy@ or @q dx₁ dy₁ dx dy@ draws a
             -- quadratic Bézier curve from /current point/ @(x, y)@
             -- to /end point/ @(x + dx, y + dy)@ with a control point
             -- at @(x + dx₁, y + dy₁)@.

             | T !Double !Double
             -- ^ Absolute smooth quadratic Bézier curve.
             --
             -- @T x y@ draws a smooth quadratic Bézier curve from
             -- /current point/ to @(x, y)@. The /control point/ is
             -- either:
             --
             --  * reflection of the /end control point/ of the
             --    previous command if the previous command was a
             --    curve
             --
             --  * /current point/ if the previous command was not a
             --    curve

             | T' !Double !Double
             -- ^ Relative smooth quadratic Bézier curve.
             --
             -- @T' dx dy@ or @t dx dy@ draws a smooth quadratic
             -- Bézier curve from /current point/ @(x, y)@ to @(x +
             -- dx, y + dy)@. The /control point/ is either:
             --
             --  * reflection of the /end control point/ of the
             --    previous command if the previous command was a
             --    curve
             --
             --  * /current point/ if the previous command was not a
             --    curve

             | A !Double !Double !Double !Bool !Bool !Double !Double
             -- ^ Absolute elliptical arc curve.
             --
             -- @A rx ry a large-arc sweep x y@ draws an arc from
             -- /current point/ to @(x, y)@ where:
             --
             --  * @rx@ and @ry@ are the x and y radii of the ellipse
             --
             --  * @a@ is the rotation of the ellipse relative to the
             --    x axis in degrees
             --
             --  * @large-arc@ draws the large arch if 'True' and the
             --    small arc if 'False'
             --
             --  * @sweep@ draws the clockwise arc if 'True' and the
             --    counterclockwise arc if 'False'

             | A' !Double !Double !Double !Bool !Bool !Double !Double
             -- ^ Relative elliptical arc curve.
             --
             -- @A' rx ry a large-arc sweep dx dy@ or @a rx ry a
             -- large-arc sweep dx dy@ draws an arc from /current
             -- point/ @(x, y)@ to @(x + dx, y + dy)@ where:
             --
             --  * @rx@ and @ry@ are the x and y radii of the ellipse
             --
             --  * @a@ is the rotation of the ellipse relative to the
             --    x axis in degrees
             --
             --  * @large-arc@ draws the large arch if 'True' and the
             --    small arc if 'False'
             --
             --  * @sweep@ draws the clockwise arc if 'True' and the
             --    counterclockwise arc if 'False'

             | Z
             -- ^ Closes the curve. Draws a straight line from
             -- /current point/ to the start of the curve if the
             -- points are not the same.
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance ToAttributeValue Command where
  toAttributeValue = Text.pack . \case
    M x y                      -> printf "M %f,%f" x y
    M' x y                     -> printf "m %f,%f" x y
    L x y                      -> printf "L %f,%f" x y
    L' x y                     -> printf "l %f,%f" x y
    H x                        -> printf "H %f" x
    H' x                       -> printf "h %f" x
    V x                        -> printf "V %f" x
    V' x                       -> printf "v %f" x
    C x₁ y₁ x₂ y₂ x y          -> printf "C %f,%f %f,%f %f,%f" x₁ y₁ x₂ y₂ x y
    C' x₁ y₁ x₂ y₂ x y         -> printf "c %f,%f %f,%f %f,%f" x₁ y₁ x₂ y₂ x y
    S x₂ y₂ x y                -> printf "S %f,%f %f,%f" x₂ y₂ x y
    S' x₂ y₂ x y               -> printf "s %f,%f %f,%f" x₂ y₂ x y
    Q x₁ y₁ x y                -> printf "Q %f,%f %f,%f" x₁ y₁ x y
    Q' x₁ y₁ x y               -> printf "q %f,%f %f,%f" x₁ y₁ x y
    T x y                      -> printf "T %f,%f" x y
    T' x y                     -> printf "t %f,%f" x y
    A rx ry θ large sweep x y  ->
      printf "A %f %f %f %f %f,%f" rx ry θ (toF large) (toF sweep) x y
    A' rx ry θ large sweep x y ->
      printf "a %f %f %f %f %f,%f" rx ry θ (toF large) (toF sweep) x y
    Z                          -> "Z"
    where toF :: Bool -> Double
          toF = bool 0 1

-- | Apply a function to every point in the command (+ @rx@ and @ry@
-- for elliptic curves).
--
-- This does not affect the angle or @large-arc@/@sweep@ flags for
-- elliptic curves (@A@ and @a@).
mapCommand :: (Double -> Double) -> Command -> Command
mapCommand f = \case
  M x y                      -> M (f x) (f y)
  M' x y                     -> M' (f x) (f y)
  L x y                      -> L (f x) (f y)
  L' x y                     -> L' (f x) (f y)
  H x                        -> H (f x)
  H' x                       -> H' (f x)
  V y                        -> V (f y)
  V' y                       -> V' (f y)
  C x₁ y₁ x₂ y₂ x y          -> C (f x₁) (f y₁) (f x₂) (f y₂) (f x) (f y)
  C' x₁ y₁ x₂ y₂ x y         -> C' (f x₁) (f y₁) (f x₂) (f y₂) (f x) (f y)
  S x₂ y₂ x y                -> S (f x₂) (f y₂) (f x) (f y)
  S' x₂ y₂ x y               -> S' (f x₂) (f y₂) (f x) (f y)
  Q x₁ y₁ x y                -> Q (f x₁) (f y₁) (f x) (f y)
  Q' x₁ y₁ x y               -> Q' (f x₁) (f y₁) (f x) (f y)
  T x y                      -> T (f x) (f y)
  T' x y                     -> T' (f x) (f y)
  A rx ry θ large sweep x y  -> A (f rx) (f ry) θ large sweep (f x) (f y)
  A' rx ry θ large sweep x y -> A' (f rx) (f ry) θ large sweep (f x) (f y)
  Z                          -> Z

-- | Uniformly scale a command by a scaling factor.
--
-- This means multiplying every point (+ @rx@ and @ry@ for elliptic
-- curves) by the scaling factor.
scaleCommand :: Double
                -- ^ scaling factor
             -> Command
             -> Command
scaleCommand α = mapCommand (* α)

-- ^ Relative move.
--
-- @m dx dy@ shifts the /current point/ @(x, y)@ to @(x + dx, y +
-- dy)@.
m :: Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
m = M'

-- ^ Relative line.
--
-- @l dx dy@ draws a line from /current point/ @(x, y)@ to @(x + dx, y
-- + dy)@.
l :: Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
l = L'

-- ^ Relative horizontal line.
--
-- @h dx@ draws a line from /current point/ @(x, y)@ to @(x + dx, y)@.
h :: Double
    -- ^ dx
  -> Command
h = H'

-- ^ Relative vertical line.
--
-- @v dy@ draws a line from /current point/ @(x, y)@ to @(x, y + dy)@.
v :: Double
     -- ^ dy
  -> Command
v = V'

-- ^ Relative cubic Bézier curve.
--
-- @c dx₁ dy₁ dx₂ dy₂ dx dy@ draws a Bézier curve from /current point/
-- @(x, y)@ to /end point/ @(x + dx, y + dy)@ with a /start control
-- point/ at @(x + dx₁, y + dy₁)@ and an /end control point/ at @(x +
-- dx₂, y + dy₂)@.
c :: Double
     -- ^ dx₁
  -> Double
     -- ^ dy₁
  -> Double
     -- ^ dx₂
  -> Double
     -- ^ dy₂
  -> Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
c = C'

-- ^ Relative smooth Bézier curve.
--
-- @s dx₂ dy₂ dx dy@ draws a smooth Bézier curve from /current point/
-- @(x, y)@ to /end point/ @(x + dx, y + dy)@, with an /end control
-- point/ at @(x + dx₂, y + dy₂)@. The /start control point/ is
-- either:
--
--  * reflection of the /end control point/ of the previous command if
--    the previous command was a curve
--
--  * /current point/ if the previous command was not a curve
s :: Double
     -- ^ dx₂
  -> Double
     -- ^ dy₂
  -> Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
s = S'

-- ^ Relative quadratic Bézier curve.
--
-- @q dx₁ dy₁ dx dy@ draws a quadratic Bézier curve from /current
-- point/ @(x, y)@ to /end point/ @(x + dx, y + dy)@ with a control
-- point at @(x + dx₁, y + dy₁)@.
q :: Double
     -- ^ dx₁
  -> Double
     -- ^ dy₁
  -> Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
q = Q'

-- ^ Relative smooth quadratic Bézier curve.
--
-- @t dx dy@ draws a smooth quadratic Bézier curve from /current
-- point/ @(x, y)@ to @(x + dx, y + dy)@. The /control point/ is
-- either:
--
--  * reflection of the /end control point/ of the previous command if
--    the previous command was a curve
--
--  * /current point/ if the previous command was not a curve
t :: Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
t = T'

-- ^ Relative elliptical arc curve.
--
-- @a rx ry a large-arc sweep dx dy@ draws an arc from /current point/
-- @(x, y)@ to @(x + dx, y + dy)@ where:
--
--  * @rx@ and @ry@ are the x and y radii of the ellipse
--
--  * @a@ is the rotation of the ellipse relative to the x axis in
--    degrees
--
--  * @large-arc@ draws the large arch if 'True' and the small arc if
--    'False'
--
--  * @sweep@ draws the clockwise arc if 'True' and the
--    counterclockwise arc if 'False'
a :: Double
     -- ^ rx
  -> Double
     -- ^ ry
  -> Double
     -- ^ a
  -> Bool
     -- ^ large-arc
  -> Bool
     -- ^ sweep
  -> Double
     -- ^ dx
  -> Double
     -- ^ dy
  -> Command
a = A'

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

-- * Haskell Logo in SVG

-- | Parameters for the Haskell logo.
--
-- The Haskell (@>λ=@) has three parts:
--
--  * a left angle bracket (@>@)
--  * a lambda (@λ@)
--  * an equals sign (@=@) cut off by the lambda
--
-- These settings control the dimensions of these parts and how they
-- relate to each other.
--
-- Defaults based on examples in Wiki—but the examples weren't all
-- identical, and the Haskell code in the Wiki seems to default to
-- different parameters from the SVG or TikZ code, if I understood it
-- correctly.
data Haskell = Haskell
  { lambdaWidth :: Double
    -- ^ Width of the > and λ strokes

  , equalsWidth :: Double
    -- ^ Width of the lines in the =

  , spacing     :: Double
    -- ^ Horizontal space between > and λ as well as λ and =

  , height      :: Double
    -- ^ Total height of logo.
    --
    -- The > and λ extend for the entire width of the logo.
    --
    -- The = is vertically centered within the logo, with @spacing@
    -- vertical space between the two lines.

  , width       :: Double
    -- ^ Total width of logo.
    --
    -- The width of the logo does not affect the width of the > and λ,
    -- which are always made up of diagonal lines at a 2/3 slope. The
    -- total width of the > and λ is only affected by @lineWidth@.
    --
    -- The = extends all the way to the righthand side of the logo.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | Given a scale, these parameters match up the examples in the
-- Wiki, although the examples do not seem to be 100%
-- consistent. Using these parameters should give you something
-- indistinguishable from the "official" Haskell logo.
defaultHaskell :: Double
                  -- ^ Scaling factor, where 1 corresponds to @spacing
                  -- = 1@, making @lambdaWidth = 3@, @height = 12@ and
                  -- @width = 17@.
               -> Haskell
defaultHaskell α = Haskell
  { lambdaWidth = 3 * α
  , equalsWidth = 2 * α
  , spacing = α
  , height = 12 * α
  , width = 17 * α
  }

-- | The four paths for each part of the Haskell logo.
data HaskellPaths = HaskellPaths
  { leftAngle  :: Path
  -- ^ The >
  , lambda     :: Path
  -- ^ The λ
  , topLine    :: Path
  -- ^ Top of the =
  , bottomLine :: Path
  -- ^ Bottom of the =
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | Generate SVG paths for a Haskell logo with the given settings.
haskellPaths :: Haskell -> HaskellPaths
haskellPaths Haskell {..} = HaskellPaths { leftAngle, lambda, topLine, bottomLine }
  where leftAngle =
          [ M 0 0
          , l halfX halfY
          , l (-halfX) halfY
          , h lambdaWidth
          , l halfX (-halfY)
          , l (-halfX) (-halfY)
          , Z
          ]

        lambda =
          [ M (lambdaWidth + spacing) 0
          , h lambdaWidth
          , l ((2/3) * height) height
          , h (-lambdaWidth)
          , l (-legX) (-legY)
          , l (-legX) legY
          , h (-lambdaWidth)
          , l halfX (-halfY)
          , Z
          ]

        topLine =
          [ M (cornerX topY) topY
          , l ((2/3) * equalsWidth) equalsWidth
          , H width
          , v (-equalsWidth)
          , Z
          ]

        bottomLine =
          [ M (cornerX bottomY) bottomY
          , l ((2/3) * equalsWidth) equalsWidth
          , H width
          , v (-equalsWidth)
          , Z
          ]

        halfX = (2/3) * halfY
        halfY = height / 2

        -- the bottom part of the lambda is 3/8 of the total height
        legY = (5/16) * height
        legX = (2/3) * legY

        -- the y coordinate of the /top/ of each line in the =
        topY = (height / 2) - (spacing / 2) - equalsWidth
        bottomY = (height / 2) + (spacing / 2)

        -- each line in the = is 1 unit away from the 2/3 diagonal of
        -- the λ, which makes the x position of their top-left corner
        -- a function of their y position (calculated above)
        cornerX topLeft = (2 * lambdaWidth) + (2 * spacing) + ((2/3) * topLeft)

-- | An SVG element with paths for a Haskell logo.
--
-- Shape based on the SVG + Tikz versions of the logo from the Haskell
-- Wiki: https://wiki.haskell.org/TW-Logo-Haskell
--
-- Colors taken from Haskell.org logo
haskell :: forall m t. Dom t m => Haskell -> m (Svg t)
haskell config = fst <$> svgAttr' "svg" [] do
  let HaskellPaths {..} = haskellPaths config
  path (pure leftAngle) (pure [("fill", "#453a62")])
  path (pure lambda) (pure [("fill", "#5e5086")])
  path (pure topLine) (pure [("fill", "#8f4e8b")])
  path (pure bottomLine) (pure [("fill", "#8f4e8b")])

main :: IO ()
main = withCss "css/ui-demo.css" (Runnable $ haskell $ defaultHaskell 5)
