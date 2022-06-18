{-# LANGUAGE PatternSynonyms #-}
-- | Values for CSS properties generally use the same lexical syntax
-- as HTML/SVG attribute values, except CSS properties can also always
-- be one of several __global values__:
--
--  * @inherit@
--  * @initial@
--  * @revert@
--  * @revert-layer@
--  * @unset@
--
-- This is expressed in Haskell through the 'Css' type and the
-- 'AsAttributeValue' class.
module UI.Css.Values
  ( Css (..)

  , Length
  , px
  , RelativeLength
  , Factor

  , Angle (Rad, Deg, Turn, Grad)

  , Duration
  , ms
  , s
  )
where

import           Data.Hashable         (Hashable)
import           Data.Text             (Text)
import           Data.Text.Display     (Display (..))
import           Data.Vector.Instances ()

import           GHC.Generics          (Generic)

import           UI.Attributes         (AsAttributeValue (..))

-- * CSS Values

-- | A CSS value of the given type.
--
-- This can either be a value of the underlying type /or/ one of CSS's
-- __global values__:
--
--  * @inherit@
--  * @initial@
--  * @revert@
--  * @revert-layer@
--  * @unset@
data Css a = Value a
           | Inherit
           | Initial
           | Revert
           | RevertLayer
           | Unset
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance AsAttributeValue a => AsAttributeValue (Css a) where
  toAttributeValue = \case
    Value a     -> toAttributeValue a
    Inherit     -> "inherit"
    Initial     -> "initial"
    Revert      -> "revert"
    RevertLayer -> "revert-layer"
    Unset       -> "unset"

  fromAttributeValue = \case
    "inherit"      -> Just Inherit
    "initial"      -> Just Initial
    "revert"       -> Just Revert
    "revert-layer" -> Just RevertLayer
    "unset"        -> Just Unset
    other          -> Value <$> fromAttributeValue other

-- * CSS Units

              -- TODO: Redesign CSS unit handling?

-- ** Length

-- | CSS @<length>@ units like @px@, @cm@ and @rem@.
type Length = Text

-- | A 'Double' in @px@.
px :: Double -> Length
px x = toAttributeValue x <> "px"

-- | CSS @<length>@ or @<percentage>@.
type RelativeLength = Text

-- | A number or a %.
type Factor = Text

-- ** Angle

-- | CSS @<angle>@ units for specifying rotations.
--
-- Angles can be manipulated using any of the normal CSS units in
-- Haskell, but will always convert to radians in CSS.
--
-- >>> toCss (Rad pi)
-- "3.141592653589793rad"
--
-- >>> import Data.Text.Display (display)
-- >>> display (Rad (2 * pi)) == "2.0π rad"
-- True
newtype Angle = Rad Double
 deriving stock (Show, Eq, Ord, Generic)
 deriving anyclass (Hashable)
 deriving newtype (Num, Fractional, Floating, Real, RealFloat, RealFrac)

instance Display Angle where
  displayBuilder (Rad θ) = displayBuilder (θ / pi) <> "π rad"

-- | An 'Angle' in degrees.
--
-- >>> Deg 90
-- Rad 1.5707963267948966
--
-- >>> let Deg θ = Rad (pi / 2) in θ
-- 90.0
--
-- >>> Deg 180 == Rad pi
-- True
pattern Deg :: Double -> Angle
pattern Deg θ <- Rad ((* (180 / pi)) -> θ)
  where Deg θ = Rad (θ * pi / 180)

-- | An 'Angle' in turns (1 turn = 360° = 2π rad).
--
-- >>> Turn 1 == Rad (2 * pi)
-- True
--
-- >>> let Turn n = Deg 720 in n
-- 2.0
--
-- >>> import Data.Text.Display (display)
-- >>> display (Turn 0.5) == "1.0π rad"
-- True
pattern Turn :: Double -> Angle
pattern Turn θ <- Rad ((/ (2 * pi)) -> θ)
  where Turn θ = Rad (2 * pi * θ)

-- | An 'Angle' in gradians (1 grad = 0.9° = π/200 rad).
--
-- >>> Grad 100 == Deg 90
-- True
--
-- >>> let Grad θ = Turn 3 in θ
-- 1200.0
--
-- >>> import Data.Text.Display (display)
-- >>> display (Grad 1200) == "6.0π rad"
-- True
pattern Grad :: Double -> Angle
pattern Grad θ <- Rad ((* (200 / pi)) -> θ)
  where Grad θ = Rad (θ * pi / 200)

instance AsAttributeValue Angle where
  toAttributeValue (Rad d) = toAttributeValue d <> "rad"

  fromAttributeValue = error "CSS parsing not implemented yet"
    -- TODO: parse angles correctly!

-- ** Time

-- | A duration, stored in milliseconds.
newtype Duration = Duration Double
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Read, Num, Real, Fractional, Floating)
  deriving anyclass (Hashable)

instance AsAttributeValue Duration where
  toAttributeValue (Duration d) = toAttributeValue d <> "ms"

  fromAttributeValue = error "CSS parsing not implemented yet"
    -- TODO: parse angles correctly!

-- | A duration in milliseconds.
--
-- >>> toCss (ms 1000)
-- "1000.0ms"
ms :: Double -> Duration
ms = Duration

-- | A duration in seconds. This is converted to milliseconds in the
-- generated CSS.
--
-- >>> toCss (s 10.5)
-- "10500.0ms"
s :: Double -> Duration
s = Duration . (* 1000)
