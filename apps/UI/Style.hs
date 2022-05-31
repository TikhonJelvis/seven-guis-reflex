{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Functionality for working with element's @style@ attributes as
-- well as their /computed/ styles.
module UI.Style
  ( ToCss

  , Length
  , px
  , RelativeLength
  , Factor
  , Angle (..)
  , Duration (..)
  , ms
  , s
  , styles
  , joinStyles
  , setProperty
  , updateProperty
  , setUserSelect

  , Transform (..)
  , addTransform
  , setTransform
  , translate
  , rotate
  , scale

  , Transition (..)
  , transition
  , removeTransition

  , getComputedProperty
  )
where

import           Data.Default.Class            (Default (..))
import           Data.Foldable                 (toList)
import           Data.Functor                  ((<&>))
import           Data.Hashable                 (Hashable)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Text.Display             (Display)
import           Data.Vector                   (Vector)
import           Data.Vector.Instances         ()

import           GHC.Generics                  (Generic)

import qualified GHCJS.DOM                     as GHCJS
import qualified GHCJS.DOM.CSSStyleDeclaration as CSSStyleDeclaration
import           GHCJS.DOM.Types               (MonadDOM)
import           GHCJS.DOM.Window              as Window

import           UI.IsElement                  (IsElement, rawElement)
import           UI.Point                      (Point (..), Point3D (..))

-- * CSS Properties

-- | A CSS property name.
--
-- In the future this is going to be something more structured, but
-- for now a newtype will do.
newtype Property = Property Text
  deriving stock (Generic)
  deriving newtype (Show, Eq, ToCss, Display, IsString, Hashable)

-- * CSS Values

-- | Types that can be converted to CSS values: numbers, length units,
-- percentages... etc.
class ToCss a where
  -- | Convert the given value to valid CSS syntax.
  toCss :: a -> Text

-- | "Backdoor" instance for working with values that don't have
-- structured support in the library. Will be conveted as is, with no
-- extra quoting/etc.
instance ToCss Text where toCss = id

instance ToCss Double where toCss  = Text.pack . show
instance ToCss Int where toCss     = Text.pack . show
instance ToCss Integer where toCss = Text.pack . show

instance ToCss a => ToCss [a] where
  toCss = Text.intercalate " " . map toCss

-- | Convert to @(x, y)@.
--
-- >>> toCss (Point 1 2)
-- "(1.0, 2.0)"
instance ToCss Point where
  toCss Point { x, y } = "(" <> toCss x <> ", " <> toCss y <> ")"

-- | Convert to @(x, y, z)@.
--
-- >>> toCss (Point3D 1 2 3)
-- "(1.0, 2.0, 3.0)"
instance ToCss Point3D where
  toCss Point3D { x, y, z } =
    "(" <> toCss x <> ", " <> toCss y <> ", " <> toCss z <> ")"

-- * Units

              -- TODO: Redesign CSS unit handling?

-- | CSS @<length>@ units like @px@, @cm@ and @rem@.
type Length = Text

-- | A 'Double' in @px@.
px :: Double -> Length
px x = toCss x <> "px"

-- | CSS @<length>@ or @<percentage>@.
type RelativeLength = Text

-- | A number or a %.
type Factor = Text

-- | CSS @<angle>@ units for specifying rotations.
data Angle = Deg !Double
           | Grad !Double
           | Rad !Double
           | Turn !Double
 deriving stock (Show, Eq, Generic)
 deriving anyclass (Hashable)

instance ToCss Angle where
  toCss = \case
    Deg d  -> toCss d <> "deg"
    Grad d -> toCss d <> "grad"
    Rad d  -> toCss d <> "rad"
    Turn d -> toCss d <> "turn"

-- | A duration, stored in milliseconds.
newtype Duration = Duration Double
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Read, Num, Real, Fractional, Floating)
  deriving anyclass (Hashable)

instance ToCss Duration where
  toCss (Duration d) = toCss d <> "ms"

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

    -- TODO: Structured representation for easing functions?
-- | An @<easing-function>@ specifying how to interpolate between
-- values.
--
-- See MDN:
-- [<easing-function>](https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function)
type EasingFunction = Text

-- * Style Attribute

-- | Parse out the CSS declarations defined in a style attribute
-- string.
--
-- Warning: This isn't a full CSS parser—it doesn't handle escaped :
-- or ; chracters or comments.
--
-- >>> styles "margin: 15px; line-height: 1.5; text-align: center;"
-- fromList [("line-height","1.5"),("margin","15px"),("text-align","center")]
--
-- >>> styles ""
-- fromList []
styles :: Text -> Map Text Text
styles attribute = Map.fromList
  [ toKV declaration
  | declaration <- Text.strip <$> Text.split (== ';') attribute
  , declaration /= ""
  ]
  where toKV declaration =
          let (a, b) = Text.break (== ':') declaration
          in (Text.strip a, Text.strip $ Text.drop 1 b)

-- | Combine a map of CSS declarations into a single string that can
-- be used in a @style@ attribute.
--
-- Declarations will be included in alphabetical order by property
-- name.
joinStyles :: Map Text Text -> Text
joinStyles = Text.intercalate "; " . map joinDeclaration . Map.toList
  where joinDeclaration (property, value) = property <> ": " <> value

-- | Set a property to a value in a set of attributes.
--
-- Adds a @style@ attribute if one is not present, otherwise modifies
-- the existing @style@ value in place.
--
-- This will overwrite the previous value /of that exact property/,
-- but will not touch related properties. Setting @border: 1px@ will
-- override the previous @border@ value but will not affect properties
-- like @border-width@ or @border-right@.
--
-- >>> setProperty "color" "blue" [("style", "pointer: auto")]
-- fromList [("style","color: blue; pointer: auto")]
--
-- >>> setProperty "color" "blue" [("style", "pointer: auto; color: green; background-color: yellow")]
-- fromList [("style","background-color: yellow; color: blue; pointer: auto")]
--
-- >>> setProperty "color" "blue" []
-- fromList [("style","color: blue")]
setProperty :: Property
            -- ^ Property name
            -> Text
            -- ^ Property value
            -> Map Text Text
            -> Map Text Text
setProperty (Property property) value attributes = case Map.lookup "style" attributes of
  Nothing       -> Map.insert "style" (joinStyles [(property, value)]) attributes
  Just existing -> Map.insert "style" (joinStyles $ update existing) attributes
  where update = Map.insert property value . styles

-- | Use the given function to combine the old value of a property
-- with the given new value.
--
-- If the property is not set, this will set it to the given value.
--
-- This is handy for properties that can take a sequence of values
-- like @transform@:
--
-- >>> let app b a = a <> " " <> b
-- >>> let translate = "translate(10px, 10px)"
--
-- >>> updateProperty app "transform" translate []
-- fromList [("style","transform: translate(10px, 10px)")]
--
-- >>> updateProperty app "transform" translate [("style", "transform: rotate(10deg)")]
-- fromList [("style","transform: rotate(10deg) translate(10px, 10px)")]
updateProperty :: (Text -> Text -> Text)
               -- ^ Function to combine values: @f new old@
               -> Property
               -- ^ Property name
               -> Text
               -- ^ New property value
               -> Map Text Text
               -> Map Text Text
updateProperty f (Property property) value attributes =
  case Map.lookup "style" attributes of
    Nothing       -> Map.insert "style" (joinStyles [(property, value)]) attributes
    Just existing -> Map.insert "style" (joinStyles $ update existing) attributes
  where update = Map.insertWith f property value . styles

-- | Set the @user-select@ and @-webkit-user-select@ properties.
--
-- While WebKit /should/ support @user-select@ with no prefix, only
-- the prefixed version worked for me in WebKitGTK.
setUserSelect :: Text -> Map Text Text -> Map Text Text
setUserSelect value =
  setProperty "user-select" value . setProperty "-webkit-user-select" value

-- * CSS Transforms

-- $ The CSS @transform@ property lets you translate, scale, rotate
-- and skew how an element is rendered in three dimensions. The value
-- of a @transform@ property can specify /multiple/ __transform
-- functions__ that are combined into a single transformation.
--
-- See MDN:
--
--  * [transform](https://developer.mozilla.org/en-US/docs/Web/CSS/transform)
--  * [<transform-function>](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function)

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

               | Rotate3D !Point3D !Angle
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

               | Scale3D !Point3D
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
-- >>> toCss (Rotate3D (Point3D 1 2 3) (Deg 10))
-- "rotate3d(1.0, 2.0, 3.0, 10.0deg)"
--
-- >>> toCss (Scale "2" "10%")
-- "scale(2, 10%)"
--
-- >>> toCss (Scale3D (Point3D 1 2 3))
-- "scale3d(1.0, 2.0, 3.0)"
--
-- >>> toCss (Skew (Deg 5) (Deg 7))
-- "skew(5.0deg, 7.0deg)"
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
    Rotate3D Point3D { x, y, z } a ->
      "rotate3d(" <> commas [toCss x, toCss y, toCss z, toCss a] <> ")"
    Scale x y ->
      "scale(" <> commas [x, y] <> ")"
    Scale3D Point3D { x, y, z } ->
      "scale3d(" <> commas [x, y, z] <> ")"
    Skew ax ay ->
      "skew(" <> commas [ax, ay] <> ")"
    Translate x y z ->
      "translate3d(" <> commas [x, y, z] <> ")"
    where commas :: ToCss a => [a] -> Text
          commas = Text.intercalate ", " . map toCss

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
translate :: Point -> Map Text Text -> Map Text Text
translate Point { x, y } = addTransform $ Translate (px x) (px y) "0"

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

-- * Transitions

-- | The CSS @transition@ property lets us animate changes in values
-- of other properties like colors, sizes and positions.
data Transition = Transition
  { property :: Text
    -- ^ The name of the CSS property to transition. The animation
    -- will be applied when the given property changes.

  , duration :: Duration
    -- ^ How long the transition takes.

  , timing   :: EasingFunction
    -- ^ An easing function for computing the intermediate values
    -- during a transition.
    --
    -- See MDN
    -- [<easing-function>](https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function)

           -- TODO: support global values like "revert" and "unset"
  , delay    :: Duration
    -- ^ How long to wait before starting a transition. During the
    -- initial delay, the corresponding property will not change.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance ToCss Transition where
  toCss Transition { property, duration, timing, delay } =
    Text.intercalate " " [toCss property, toCss duration, toCss timing, toCss delay]

instance Default Transition where
  def = Transition { property = "all", duration = s 0, timing = "ease", delay = s 0 }

-- | Add a transition for the given property, without overriding any
-- existing transitions on the element.
transition :: Transition -> Map Text Text -> Map Text Text
transition = updateProperty after "transition" . toCss
  where after new existing = existing <> ", " <> new

-- | Remove any transitions set for the current property.
removeTransition :: Property -> Map Text Text -> Map Text Text
removeTransition (Property property) =
  updateProperty remove "transition" "" . updateProperty remove' "transition-property" ""
  where remove _ = overProperties $ filter (not . Text.isInfixOf property)
        remove' _ = overProperties $ filter (/= property)

        overProperties f = Text.intercalate ", " . f . parseProperties
        parseProperties = map Text.strip . Text.split (== ',')

-- * Computed Styles

-- $ Functions for working with the /computed/ style of DOM
-- elements. The actual values of a CSS property on a DOM element are
-- combined from several sources:
--
--  * the element's @style@ attribute (@element.style@ in JavaScript)
--  * rules matching the element from external stylesheets
--  * intermediate calculations from CSS animations and transitions
--
-- This means that inspecting an element's @style@ attribute is not
-- sufficient to know what the element is /actually/ styled
-- as. Instead, we can get this information by querying the element's
-- __computed style__ directly.
--
-- Apart from accounting for the different ways an element's property
-- can be set, computed properties are also evaluated to __resolved
-- values__:
--
--  * relative units like @em@ are converted to absolute units like
--    @px@
--  * special properties like @inherit@ are converted to normal values
--  * variables and computations like animations are evaluated
--
-- These additional calculations mean that even if a property is
-- explicitly set in an element's @style@ attribute—which has a higher
-- priority than rules from external stylesheets—the computed value
-- can still differ from the specified value.
--
-- See MDN:
--
--  * [Window.getComputedStyle](https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle)
--  * [resolved value](https://developer.mozilla.org/en-US/docs/Web/CSS/resolved_value)
--  * [computed value](https://developer.mozilla.org/en-US/docs/Web/CSS/computed_value)
--  * [used value](https://developer.mozilla.org/en-US/docs/Web/CSS/used_value)

        -- TODO: error handling!
-- | Get the __computed value__ of a specific style property.
--
-- Returns @Nothing@ if the property name is not valid.
--
-- Note: the underlying @Window.getComputedStyle@ API does not support
-- shorthand properties. This function will account for that in the
-- future, but for now asking for a shorthand property will produce
-- @Nothing@.
getComputedProperty :: (IsElement e, MonadDOM m)
                    => e
                    -- ^ Element to get the computed property for
                    -> Property
                    -- ^ The name of the property
                    -> m (Maybe Text)
                    -- ^ The computed value of the property as a string
                    -- or 'Nothing' if the property name is not supported
getComputedProperty (rawElement -> element) (Property propertyName) = do
  window <- GHCJS.currentWindow <&> \case
    Just w  -> w
    Nothing -> error "Could not get global Window object"
  styleDeclaration <- Window.getComputedStyle window element (Nothing @Text)
  Just <$> CSSStyleDeclaration.getPropertyValue styleDeclaration propertyName
