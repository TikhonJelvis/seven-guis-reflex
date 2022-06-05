module UI.Point where

import           Data.Hashable     (Hashable)
import           Data.Text.Display

import           GHC.Generics      (Generic)

-- * 2D

-- | A point in 2D space.
--
-- The semantics of a point depend on context: it could be an absolute
-- position relative to some origin or a vector representing a
-- translation between two points.
data Point = Point { x, y :: !Double }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | Display the point as an ordered pair:
--
-- >>> display (Point 5 10)
-- "(5.0, 10.0)"
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

-- | The Euclidean distance between two points.
distance :: Point -> Point -> Double
distance (Point x₁ y₁) (Point x₂ y₂) =
  sqrt $ ((x₂ - x₁) ** 2) + ((y₂ - y₁) ** 2)

-- * 3D

-- | A point in 3D space.
--
-- The semantics of a point depend on context: it could be an absolute
-- position relative to some origin or a vector representing a
-- translation between two points.
data Point3D = Point3D { x, y, z :: !Double }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | Display the point as an ordered triple:
--
-- >>> display (Point3D 1 2 3)
-- "(1.0, 2.0, 3.0)"
instance Display Point3D where
  displayBuilder Point3D { x, y, z } =
    "("  <> displayBuilder x <>
    ", " <> displayBuilder y <>
    ", " <> displayBuilder z <> ")"

-- | Strictly speaking, 'Point3D' is a member of an affine space, but
-- for now we'll just treat it like a number. This means that we'll
-- implicitly treat some 'Point3D' values as vectors rather than
-- absolute points.
--
-- @fromInteger n@ gives us a point at @(n, n, n)@.
instance Num Point3D where
  fromInteger (fromInteger -> n) = Point3D {x = n, y = n, z = n }
  Point3D x₁ y₁ z₁ + Point3D x₂ y₂ z₂ = Point3D (x₁ + x₂) (y₁ + y₂) (z₁ + z₂)
  Point3D x₁ y₁ z₁ * Point3D x₂ y₂ z₂ = Point3D (x₁ * x₂) (y₁ * y₂) (z₁ * z₂)
  Point3D x₁ y₁ z₁ - Point3D x₂ y₂ z₂ = Point3D (x₁ - x₂) (y₁ - y₂) (z₁ - z₂)
  abs Point3D { x, y, z } = Point3D { x = abs x, y = abs y, z = abs z }
  signum Point3D { x, y, z } = Point3D { x = signum x, y = signum y, z = signum z }
