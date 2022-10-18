{-# LANGUAGE PatternSynonyms #-}
-- | Units for specifying lengths, times, angles... etc.
module UI.Units
  ( Percent
  , Factor

  , Length (..)
  , px
  , u
  , RelativeLength

  , Angle (Rad, Deg, Turn, Grad)

  , Duration
  , ms
  , s
  )
where

import           Data.Hashable     (Hashable)
import           Data.String       (IsString)
import           Data.Text         (Text)
import           Data.Text.Display (Display (..))

import           GHC.Generics      (Generic)

import           UI.Attributes     (AsAttributeValue (..),
                                    CombineAttributeValue, toAttributeValue)

-- TODO: More structured unit handling?

-- * Relative Measures

-- | A percentage.
type Percent = Text

-- | A number or a %.
type Factor = Text

-- * Length

-- TODO: separate <length> and <length-percent>?
-- | CSS @<length>@ and @<length-percent>@ values.
newtype Length = Length Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Hashable, CombineAttributeValue, AsAttributeValue, IsString)

-- | Lengths measured in pixels.
px :: Double -> Length
px x = Length $ toAttributeValue x <> "px"

-- | Lengths without units specified.
--
-- For SVG, this represents unitless "user space coordinates".
u :: Double -> Length
u = Length . toAttributeValue

-- | CSS @<length>@ or @<percentage>@.
type RelativeLength = Length

-- * Angle

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

instance CombineAttributeValue Angle
instance AsAttributeValue Angle where
  toAttributeValue (Rad d) = toAttributeValue d <> "rad"

  fromAttributeValue = error "CSS parsing not implemented yet"
    -- TODO: parse angles correctly!

-- * Time

-- | A duration, stored in milliseconds.
newtype Duration = Duration Double
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Read, Num, Real, Fractional, Floating)
  deriving anyclass (Hashable)

instance CombineAttributeValue Duration
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
