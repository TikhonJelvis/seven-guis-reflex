-- | CSS classes
module UI.Class
  ( ClassName (..)

  , classIf
  , classesIf
  )
where

import           Data.Bool               (bool)
import           Data.Coerce             (coerce)
import qualified Data.Foldable           as Foldable
import           Data.Hashable           (Hashable (..))
import           Data.Maybe              (fromJust)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..))

import           GHC.Generics            (Generic)

import           UI.Attributes.Attribute


-- | A single CSS class name.
--
-- __Example__
--
-- @
-- div [class_ := ["draggable"]]
-- @
--
-- would be matched by the CSS rule:
--
-- @
-- .draggable {
--   ...
-- }
-- @
newtype ClassName = ClassName Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (IsString, AsAttributeValue)

instance Display ClassName where
  displayBuilder (ClassName c) = "." <> displayBuilder c

instance AsAttributeValue (Set ClassName) where
  toAttributeValue =
    Text.intercalate " " . coerce . Foldable.toList

  fromAttributeValue =
    Just . Set.delete "" . Set.fromList . coerce . Text.split isHtmlWhitespace

instance IsString (Set ClassName) where
  fromString = fromJust . fromAttributeValue . Text.pack


-- | The given class if a condition is 'True', or an empty set
-- otherwise.
classIf :: ClassName -> Bool -> Set ClassName
classIf c = bool mempty [c]

-- | The given classes if the condition is 'True', or an empty set
-- otherwise.
classesIf :: Set ClassName -> Bool -> Set ClassName
classesIf = bool mempty
