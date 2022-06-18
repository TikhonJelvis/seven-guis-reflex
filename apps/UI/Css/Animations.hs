module UI.Css.Animations where

import           Data.Default.Class      (Default (..))
import           Data.Hashable           (Hashable)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Vector.Instances   ()

import           GHC.Generics            (Generic)

import           UI.Attributes           (AsAttributeValue)
import           UI.Attributes.Attribute (AsAttributeValue (..))
import qualified UI.Css.Rules            as Rules
import           UI.Css.Rules            (CssRules, Property (..))
import           UI.Css.Values           (Duration, s)


-- * Transitions

                 -- TODO: structured type for easing functions
type EasingFunction = Text

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

instance AsAttributeValue Transition where
  toAttributeValue Transition { property, duration, timing, delay } =
    Text.intercalate " " [ toAttributeValue property
                         , toAttributeValue duration
                         , toAttributeValue timing
                         , toAttributeValue delay
                         ]

  fromAttributeValue = error "CSS parsing not implemented yet"

instance Default Transition where
  def = Transition { property = "all", duration = s 0, timing = "ease", delay = s 0 }

-- | Add a transition for the given property, without overriding any
-- existing transitions on the element.
transition :: Transition -> CssRules -> CssRules
transition = Rules.updateProperty after "transition" . toAttributeValue
  where after new existing = existing <> ", " <> new

-- | Remove any transitions set for the current property.
removeTransition :: Property -> CssRules -> CssRules
removeTransition (Property property) =
  Rules.updateProperty remove "transition" "" .
  Rules.updateProperty remove' "transition-property" ""
  where remove _ = overProperties $ filter (not . Text.isInfixOf property)
        remove' _ = overProperties $ filter (/= property)

        overProperties f = Text.intercalate ", " . f . parseProperties
        parseProperties = map Text.strip . Text.split (== ',')
