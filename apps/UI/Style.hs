{-# LANGUAGE PatternSynonyms #-}
-- | Functionality for working with element's @style@ attributes as
-- well as their /computed/ styles.
module UI.Style
  ( ToCss (..)

  , Length
  , px

  , RelativeLength

  , Factor

  , Angle (.., Deg, Turn, Grad)

  , Duration (..)
  , ms
  , s

  , styles
  , joinStyles
  , setProperty
  , updateProperty
  , setUserSelect

  , Transition (..)
  , transition
  , removeTransition

  , BackfaceVisibility (..)
  , backfaceVisibility

  , getComputedProperty)
where

import           Data.Default.Class            (Default (..))
import           Data.Functor                  ((<&>))
import           Data.Hashable                 (Hashable)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Text.Display             (Display (..))
import           Data.Vector.Instances         ()

import           GHC.Generics                  (Generic)

import qualified GHCJS.DOM                     as GHCJS
import qualified GHCJS.DOM.CSSStyleDeclaration as CSSStyleDeclaration
import           GHCJS.DOM.Types               (MonadDOM)
import           GHCJS.DOM.Window              as Window

import           Linear                        (V2 (..), V3 (..))

import           UI.Element.IsElement          (IsElement, rawElement)

-- * Style Attribute

-- * CSS Properties

-- ** Transitions

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

-- ** Misc

data BackfaceVisibility = Visible | Hidden
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

instance ToCss BackfaceVisibility where
  toCss Visible = "visible"
  toCss Hidden  = "hidden"

-- | Set the @backface-visibility@ CSS property.
backfaceVisibility :: BackfaceVisibility -> Map Text Text -> Map Text Text
backfaceVisibility = setProperty "backface-visibility" . toCss

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
