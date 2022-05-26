{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module UI.Point where

import           Data.Text         (Text)
import           Data.Text.Display

-- | A point in 2D space, measured in px.
--
-- The origin of the point depends on context: it can be an absolute
-- position relative to the viewport or document, or it can be a point
-- relative to an element.
data Point = Point
  { x :: !Double
  -- ^ The x coordinate of the point in px.
  --
  -- Starts with 0 at the left and goes right, or, in RTL mode, starts
  -- with 0 at the right and goes left.
  , y :: !Double
  -- ^ The y coordinate of the point in px. Starts with 0 at the top
  -- and goes down.
  }
  deriving stock (Show, Eq)

-- | Display the point as an ordered pair:
--
-- >>> display (Point 5 10)
-- "(5, 10)"
instance Display Point where
  displayBuilder Point { x, y } =
    "(" <> displayBuilder x <> ", " <> displayBuilder y <> ")"

-- | Strictly speaking, 'Point' is a member of an affine space, but
-- for now we'll just treat it like a number. This means that we'll
-- implicitly treat some 'Point' values as vectors rather than
-- absolute points.
--
-- @fromInteger n@ gives us a point at @(n, n)@.
instance Num Point where
  fromInteger (fromInteger -> n) = Point { x = n, y = n }
  Point x₁ y₁ + Point x₂ y₂ = Point (x₁ + x₂) (y₁ + y₂)
  Point x₁ y₁ * Point x₂ y₂ = Point (x₁ * x₂) (y₁ * y₂)
  Point x₁ y₁ - Point x₂ y₂ = Point (x₁ - x₂) (y₁ - y₂)
  abs Point { x, y } = Point { x = abs x, y = abs y }
  signum Point { x, y } = Point { x = signum x, y = signum y }

-- | Produce a 'point' from two numbers.
point :: forall a. Real a => a -> a -> Point
point (toRational -> x) (toRational -> y) =
  Point { x = fromRational x, y = fromRational y }
