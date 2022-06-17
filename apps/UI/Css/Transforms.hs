-- | The CSS @transform@ property lets you translate, scale, rotate
-- and skew how an element is rendered in three dimensions. The value
-- of a @transform@ property can specify /multiple/ __transform
-- functions__ that are combined into a single transformation.
--
-- See MDN:
--
--  * [transform](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
--  * [<transform-function>](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function)
module UI.Css.Transforms where

import           Data.Default.Class            (Default (..))
import           Data.Foldable                 (toList)
import           Data.Functor                  ((<&>))
import           Data.Hashable                 (Hashable)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Text.Display             (Display (..))
import           Data.Vector                   (Vector)
import           Data.Vector.Instances         ()

import           GHC.Generics                  (Generic)

import qualified GHCJS.DOM                     as GHCJS
import qualified GHCJS.DOM.CSSStyleDeclaration as CSSStyleDeclaration
import           GHCJS.DOM.Types               (MonadDOM)
import           GHCJS.DOM.Window              as Window

import           Linear                        (V2 (..), V3 (..))

import           Text.Printf                   (printf)

import           UI.Attributes.Attribute       (AsAttributeValue (..))
import           UI.Element.IsElement          (IsElement, rawElement)

-- | CSS supports a number of different __transform functions__. Each
-- of these functions can be expressed as a matrix (with 'Matrix3D'),
-- but using more specific functions can be more intuitive and
-- supports different units.
data Transform = Matrix3D !(Vector Double)
               -- ^ The @matrix3d@ transform function. Needs 16
               -- values.
               --
               -- See MDN:
               -- [matrix3d](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/matrix3d)

               | Perspective !Length
               -- ^ Transforms the element as if it is on the XY plane
               -- seen from the given distance along the Z axis.
               --
               -- Note: if @perspective@ is specified along with other
               -- transform functions, it must be listed first.
               --
               -- See MDN:
               -- [perspective](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/perspective)

               | Rotate !Angle
               -- ^ Rotate an element in 2D space around its
               -- @transform-origin@.
               --
               -- See MDN:
               --  * [rotate](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/rotate)
               --  * [transform-origin](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin)

               | Rotate3D !(V3 Double) !Angle
               -- ^ Rotate the element in 3D around an axis of
               -- rotation (specified as an @(x, y, z)@ point).
               --
               -- See MDN:
               -- [rotate3d](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/rotate3d)

               | Scale !Factor !Factor
               -- ^ Scale in two dimensions by the given factors
               -- (either numbers or percent).
               --
               -- See MDN:
               -- [scale3](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/scale3)

               | Scale3D !(V3 Double)
               -- ^ Scale the element by the given factor along the X,
               -- Y and Z axes respectively.
               --
               -- See MDN:
               -- [scale3d](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/scale3d)

               | Skew !Angle !Angle
               -- ^ Skew an element on the 2D plane.
               --
               -- See MDN:
               -- [skew](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/skew)

               | Translate !RelativeLength !RelativeLength !Length
               -- ^ Translate an element in 3D space.
               --
               -- The X and Y translations can be specified in @%@
               -- which will be computed relative to the element's
               -- parent.
               --
               -- See MDN:
               -- [translate3d](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/translate3d)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | To the corresponding @<transform-function>@.
--
-- >>> toCss (Matrix3D [1..16])
-- "matrix3d(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)"
--
-- >>> toCss (Perspective "10cm")
-- "perspective(10cm)"
--
-- >>> toCss (Rotate (Rad 10))
-- "rotate(10.0rad)"
--
-- >>> toCss (Rotate3D (V3 1 2 3) (Deg 10))
-- "rotate3d(1.0, 2.0, 3.0, 0.17453292519943295rad)"
--
-- >>> toCss (Scale "2" "10%")
-- "scale(2, 10%)"
--
-- >>> toCss (Scale3D (V3 1 2 3))
-- "scale3d(1.0, 2.0, 3.0)"
--
-- >>> toCss (Skew (Deg 5) (Deg 7))
-- "skew(8.726646259971647e-2rad, 0.12217304763960307rad)"
--
-- >>> toCss (Translate "10%" "5px" "10cm")
-- "translate3d(10%, 5px, 10cm)"
instance ToCss Transform where
  toCss = \case
    Matrix3D v
      | length v == 16 ->
        "matrix3d(" <> commas (toList v) <> ")"
      | otherwise      ->
        error $ "Invalid Matrix3D: needs 16 values.\nGot: " <> show v
    Perspective a ->
      "perspective(" <> toCss a <> ")"
    Rotate a ->
      "rotate(" <> toCss a <> ")"
    Rotate3D (V3 x y z) a ->
      "rotate3d(" <> commas [toCss x, toCss y, toCss z, toCss a] <> ")"
    Scale x y ->
      "scale(" <> commas [x, y] <> ")"
    Scale3D (V3 x y z) ->
      "scale3d(" <> commas [x, y, z] <> ")"
    Skew ax ay ->
      "skew(" <> commas [ax, ay] <> ")"
    Translate x y z ->
      "translate3d(" <> commas [x, y, z] <> ")"
    where commas :: ToCss a => [a] -> Text
          commas = Text.intercalate ", " . map toCss

    -- TODO: implement fromAttributeValue
instance AsAttributeValue [Transform] where
  toAttributeValue = Text.intercalate " " . map toCss
  fromAttributeValue = error "unimplemented"

  combineAttributeValues = (<>)

-- ** Setting CSS Transforms

-- | Add a transform function to a @style@ attribute.
--
-- If a @transform@ property is already set, this will add to the end
-- of that property (except for @perspective@ which is added to the
-- front).
addTransform :: Transform -> Map Text Text -> Map Text Text
addTransform = \case
  p@Perspective{} -> updateProperty before "transform" (toCss p)
  transform       -> updateProperty after "transform" (toCss transform)
  where before new old = new <> " " <> old
        after new old  = old <> " " <> new

    -- TODO: handle Perspective correctly?
-- | Set the @tranform@ property of a value, overriding any previous
-- setting.
setTransform :: [Transform] -> Map Text Text -> Map Text Text
setTransform (toCss -> transforms) = setProperty "transform" transforms

-- | Translate an element along the given X and Y distances in @px@.
translate :: V2 Double -> Map Text Text -> Map Text Text
translate (V2 x y) = addTransform $ Translate (px x) (px y) "0"

-- | Rotate an element in 2D around its @transform-origin@.
--
-- See MDN:
--  * [rotate](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function/rotate)
--  * [transform-origin](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin)
rotate :: Angle -> Map Text Text -> Map Text Text
rotate = addTransform . Rotate

-- | Uniformly scale an element.
scale :: Double -> Map Text Text -> Map Text Text
scale n = addTransform $ Scale (toCss n) (toCss n)

-- | Flip an element around an axis running through the element's
-- /center of rotation/ at an angle of θ to the y-axis.
--
-- __Examples__
--
-- Flip horizontally:
--
-- @
-- flipAround (Deg 0)
-- @
--
-- Flip vertically:
--
-- @
-- flipAround (Turn 0.25)
-- @
--
-- Flip along diagonal:
--
-- @
-- flipAround (Deg 45)
-- @
flipAround :: Angle -> Map Text Text -> Map Text Text
flipAround θ =
  addTransform (Rotate (-θ)) .
  addTransform (Scale "-1" "1") .
  addTransform (Rotate θ)

-- | Add a 2D matrix transform.
--
-- The matrix can take one of two forms:
--
-- @[a, b, c, d]@, mapping to:
--
-- @
-- matrix(a, b, c, d, 0, 0)
-- @
--
-- (no translation)
--
-- and
--
-- @[a, b, c, d, tx, ty]@
--
-- mapping to:
--
-- @
-- matrix(a, b, c, d, tx, ty)
-- @
--
-- (with translation by @(tx, ty)@)
--
-- Any other number of entries will result in a runtime error.
matrix :: Vector Double -> Map Text Text -> Map Text Text
matrix [a, b, c, d] = addTransform $
  Matrix3D [a, b, 0, 0, c, d, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]
matrix [a, b, c, d, tx, ty] = addTransform $
  Matrix3D [a, b, 0, 0, c, d, 0, 0, 0, 0, 1, 0, ty, tx, 0, 1]
matrix invalid =
  error $ printf "Invalid matrix: %s" (show invalid)
