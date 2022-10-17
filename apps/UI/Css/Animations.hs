module UI.Css.Animations where

import           Data.Default.Class      (Default (..))
import           Data.Hashable           (Hashable)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector
import           Data.Vector.Instances   ()

import           GHC.Exts                (IsList (..))
import           GHC.Generics            (Generic)

import           UI.Attributes           (AsAttributeValue, Attribute,
                                          CombineAttributeValue (..))
import           UI.Attributes.Attribute (AsAttributeValue (..))
import qualified UI.Css.Rules            as Rules
import           UI.Css.Values           (Duration, s)


-- * Transitions

-- | How to animate changes to the element's properties.
--
-- See 'Transition' for details.
transition :: Attribute Transitions
transition = Rules.css "transition"

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

instance CombineAttributeValue Transition
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

-- | All of the transitions set on an element.
newtype Transitions = Transitions (Vector Transition)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)
  deriving newtype (Semigroup, Monoid)

instance IsList Transitions where
  type Item Transitions = Transition

  toList (Transitions xs) = toList xs
  fromList = Transitions . fromList

instance CombineAttributeValue Transitions where
  combineAttributeValues (Transitions a) (Transitions b) = Transitions (a <> b)

    -- TODO: implement fromAttributeValue
instance AsAttributeValue Transitions where
  toAttributeValue (Transitions ts) =
    Text.intercalate " " . map toAttributeValue $ Vector.toList ts
  fromAttributeValue = error "unimplemented"

