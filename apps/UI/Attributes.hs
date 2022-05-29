{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}
module UI.Attributes where

import           Data.Bool          (bool)
import           Data.Default.Class (Default (..))
import           Data.Foldable      (toList)
import qualified Data.Foldable      as Foldable
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Vector        (Vector)

import           UI.Point           (Point (..), Point3D (..))

-- * Attribute Sets

-- | Types that can be converted to HTML/SVG attribute sets.
class ToAttributes a where
  toAttributes :: a -> Map Text Text

instance ToAttributeValue a => ToAttributes (Map Text a) where
  toAttributes = (toAttributeValue <$>)

-- | Apply the given attributes on top of existing attributes. The new
-- attributes will override the old attributes any time there is an
-- overlap.
with :: ToAttributes a => a -> Map Text Text -> Map Text Text
with = Map.union . toAttributes

-- * Attribute Values

-- | Types that can be converted to HTML/SVG attribute values.
class ToAttributeValue a where
  toAttributeValue :: a -> Text

instance ToAttributeValue Text where toAttributeValue = id

instance ToAttributeValue Int where toAttributeValue = Text.pack . show

instance ToAttributeValue Double where toAttributeValue = Text.pack . show

-- * Specific Attributes

-- ** Classes

-- | Does the given set of attributes have a class with the given
-- name?
--
-- >>> hasClass "selected" [("class", "my selected widget")]
-- True
--
-- >>> hasClass "selected" [("class", "my widget")]
-- False
--
-- >>> hasClass "selected" []
-- False
hasClass :: Text -> Map Text Text -> Bool
hasClass class_ attributes = case Map.lookup "class" attributes of
  Just attribute -> class_ `elem` classes attribute
  Nothing        -> False

-- | Add a class to the given set of attributes.
--
-- This function will always normalize existing class attribute by
-- dropping extra whitespace, sorting the class names and dropping any
-- duplicates.
--
-- >>> addClass "selected" []
-- fromList [("class","selected")]
--
-- >>> addClass "selected" [("class","selected   widget   selected")]
-- fromList [("class","selected widget")]
--
-- >>> addClass "selected" [("class", "my widget")]
-- fromList [("class","my selected widget")]
--
addClass :: Text -> Map Text Text -> Map Text Text
addClass = Map.insertWith add "class"
  where add class_ = joinClasses . Set.insert class_ . classes

-- | Add or remove a class based on a boolean: 'True' adds the class,
-- 'False' removes it.
--
-- >>> setClass "selected" True [("class", "my widget")]
-- fromList [("class","my selected widget")]
--
-- >>> setClass "selected" False [("class", "my selected widget")]
-- fromList [("class","my widget")]
--
setClass :: Text -> Bool -> Map Text Text -> Map Text Text
setClass class_ = bool (removeClass class_) (addClass class_)

-- | Remove the class from the given set of attributes if it is
-- present.
--
-- This function will always normalize existing class attribute by
-- dropping extra whitespace, sorting the class names and dropping any
-- duplicates.
--
-- If the resulting class attribute would be empty, the attribute is
-- removed from the set of attributes altogether.
--
-- >>> removeClass "selected" [("class", "my selected widget")]
-- fromList [("class","my widget")]
--
-- >>> removeClass "selected" [("class", "my widget")]
-- fromList [("class","my widget")]
--
-- >>> removeClass "selected" [("class","")]
-- fromList []
--
-- >>> removeClass "selected" []
-- fromList []
--
removeClass :: Text -> Map Text Text -> Map Text Text
removeClass class_ = Map.update remove "class"
  where remove attribute =
          case joinClasses $ Set.delete class_ $ classes attribute of
            ""  -> Nothing
            new -> Just new

-- | Given an entry for a @"class"@ attribute, parse out the classes
-- set by the attribute.
--
-- >>> classes "foo"
-- fromList ["foo"]
--
-- >>> classes "foo bar baz"
-- fromList ["bar","baz","foo"]
--
-- >>> classes "  foo   bar  "
-- fromList ["bar","foo"]
--
-- Note that classes can have non-ASCII whitespace as part of the
-- name:
--
-- >>> classes "bar  foo   "
-- fromList ["bar\8239\8239foo","\8239"]
--
classes :: Text
        -- ^ Class attribute
        -> Set Text
classes = Set.delete "" . Set.fromList . Text.split isHtmlWhitespace

-- | Combine a set of classes into a single attribute value, with each
-- class name separated by a single space.
--
-- >>> joinClasses (Set.fromList ["widget", "my", "selected"])
-- "my selected widget"
--
-- >>> joinClasses []
-- ""
--
joinClasses :: Foldable f => f Text -> Text
joinClasses = Text.intercalate " " . Foldable.toList

-- ** CSS Style

-- *** CSS values

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

-- *** Units

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
 deriving stock (Show, Eq)

instance ToCss Angle where
  toCss = \case
    Deg d  -> toCss d <> "deg"
    Grad d -> toCss d <> "grad"
    Rad d  -> toCss d <> "rad"
    Turn d -> toCss d <> "turn"

-- | A duration, stored in milliseconds.
newtype Duration = Duration Double
  deriving stock (Show, Eq, Ord)
  deriving newtype (Read, Num, Real, Fractional, Floating)

instance ToCss Duration where
  toCss (Duration d) = toCss d <> "ms"

    -- TODO: Structured representation for easing functions?
-- | An @<easing-function>@ specifying how to interpolate between
-- values.
--
-- See MDN:
-- [<easing-function>](https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function)
type EasingFunction = Text

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

-- *** Style Attribute

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
setProperty :: Text
            -- ^ Property name
            -> Text
            -- ^ Property value
            -> Map Text Text
            -> Map Text Text
setProperty property value attributes = case Map.lookup "style" attributes of
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
               -> Text
               -- ^ Property name
               -> Text
               -- ^ New property value
               -> Map Text Text
               -> Map Text Text
updateProperty f property value attributes = case Map.lookup "style" attributes of
  Nothing       -> Map.insert "style" (joinStyles [(property, value)]) attributes
  Just existing -> Map.insert "style" (joinStyles $ update existing) attributes
  where update = Map.insertWith f property value . styles

-- ***

-- | Set the @user-select@ and @-webkit-user-select@ properties.
--
-- While WebKit /should/ support @user-select@ with no prefix, only
-- the prefixed version worked for me in WebKitGTK.
setUserSelect :: Text -> Map Text Text -> Map Text Text
setUserSelect value =
  setProperty "user-select" value . setProperty "-webkit-user-select" value

-- *** CSS Transforms

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

-- **** Setting CSS Transforms

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

-- *** CSS Transitions

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
  deriving stock (Show, Eq)

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

-- * Attribute Parsing

-- | Is the character an HTML whitespace character?
--
-- HTML uses ASCII whitespace characters to separate class names/etc,
-- but does not treat non-ASCII whitespace specially in those cases.
--
-- See [the definition of ASCII
-- whitespace](https://infra.spec.whatwg.org/#ascii-whitespace) for
-- details.
--
-- >>> isHtmlWhitespace ' '
-- True
--
-- >>> isHtmlWhitespace 'a'
-- False
--
-- >>> isHtmlWhitespace '\8239' -- narrow no-break space
-- False
--
isHtmlWhitespace :: Char -> Bool
isHtmlWhitespace c = c `elem` htmlWhitespace

-- | The set of ASCII whitespace characters as defined by the HTML
-- standard.
--
-- HTML uses ASCII whitespace characters to separate class names/etc,
-- but does not treat non-ASCII whitespace specially in those cases.
--
-- See [the definition of ASCII
-- whitespace](https://infra.spec.whatwg.org/#ascii-whitespace) for
-- details.
--
-- >>> htmlWhitespace
-- fromList "\t\n\f\r "
--
htmlWhitespace :: Set Char
htmlWhitespace = [' ', '\t', '\n', '\f', '\r']
