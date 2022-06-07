-- | SVG paths let us describe complex curves.
--
-- This module provides a lightwieght library for writing and
-- manipulating paths.
module UI.SVG.Path where

import           Data.Hashable (Hashable)
import qualified Data.Text     as Text

import           GHC.Exts      (IsList (..))
import           GHC.Generics  (Generic)

import           Text.Printf   (printf)

import           UI.Attributes (ToAttributeValue (..), ToAttributes (..))

-- | A sequence of commands specifying a path.
newtype Path = Path { toCommands :: [Command] }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (Semigroup, Monoid)

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
      printf "A %f %f %f %d %d %f %f" rx ry θ (fromEnum large) (fromEnum sweep) x y
    A' rx ry θ large sweep x y ->
      printf "a %f %f %f %d %d %f %f" rx ry θ (fromEnum large) (fromEnum sweep) x y
    Z                          -> "Z"

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

