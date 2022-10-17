{-# LANGUAGE UndecidableInstances #-}
module UI.Css
  ( module UI.Css.Animations
  , module UI.Css.Rules
  , module UI.Css.Transforms
  , module UI.Css.Values

  , zIndex

  , borderColor
  , borderWidth
  , BorderStyle
  , borderStyle

  , BackfaceVisibility (..)
  , backfaceVisibility

  , getComputedProperty
  )
where

import           Data.Functor                  ((<&>))
import           Data.Hashable                 (Hashable)
import           Data.Text                     (Text)
import           Data.Vector.Instances         ()

import           GHC.Generics                  (Generic)

import qualified GHCJS.DOM                     as GHCJS
import qualified GHCJS.DOM.CSSStyleDeclaration as CSSStyleDeclaration
import           GHCJS.DOM.Types               (MonadDOM)
import           GHCJS.DOM.Window              as Window

import           UI.Attributes                 (AsAttributeValue, Attribute,
                                                CombineAttributeValue,
                                                Lowercase (..))
import           UI.Color                      (Color)
import           UI.Css.Animations
import           UI.Css.Rules
import           UI.Css.Transforms
import           UI.Css.Values
import           UI.Element.IsElement          (IsElement, rawElement)

-- * CSS Properties

-- | The z-index of an element controls whether it is rendered above
-- or below other elements. An element with a higher z-index will be
-- rendered above an element with a lower z-index when they overlap.
zIndex :: Attribute Int
zIndex = css "z-index"

data BackfaceVisibility = Visible | Hidden
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)
  deriving (CombineAttributeValue, AsAttributeValue) via Lowercase BackfaceVisibility

-- | Whether to render the back of the element.
backfaceVisibility :: Attribute BackfaceVisibility
backfaceVisibility = css "backface-visibility"

-- ** Borders

-- TODO: support different settings for top/bottom/left/right borders
--
-- this will probably need a change to how attributes work so that we
-- can easily talk about aggregate attributes

-- | The color of the element's border.
borderColor :: Attribute Color
borderColor = css "border-color"

-- | How wide the border of the element should be.
borderWidth :: Attribute Length
borderWidth = css "border-width"

              -- TODO: add hidden? (name conflict with
              -- BackfaceVisibility)

              -- TODO: add picture/demo to docs
-- | How the element's border is drawn.
data BorderStyle = None
                 -- ^ Don't draw the border
                 | Dotted
                 -- ^ Series of rounded dots
                 | Dashed
                 -- ^ Rectangular dashes with spaces, exact details
                 -- may differ between browsers
                 | Solid
                 -- ^ Solid line
                 | Double
                 -- ^ Two straight lines with a space between. Both
                 -- lines + space add up to the border-width.
                 | Groove
                 -- ^ Like a groove cut into the surface. Opposite of
                 -- 'Ridge'.
                 | Ridge
                 -- ^ Like a ridge extruded from the surface. Opposite
                 -- of 'Groove'.
                 | Inset
                 -- ^ Makes it look like the element is embedded in
                 -- the surface. Opposite of 'Outset'.
                 | Outset
                 -- ^ Makes it look like the element is raised from
                 -- the surface. Opposite of 'Inset'.
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)
  deriving (CombineAttributeValue, AsAttributeValue) via Lowercase BorderStyle

-- | Set the style of the border, controlling how the border is drawn.
borderStyle :: Attribute BorderStyle
borderStyle = css "border-style"

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
