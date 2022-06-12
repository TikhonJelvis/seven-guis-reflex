-- | Element ids—names that we can use to reference a specific HTML or
-- SVG element. Ids should be unique across a document.
module UI.Id
  ( Id (..)
  )
where

import           Data.Hashable           (Hashable (..))
import           Data.String             (IsString)
import           Data.Text               (Text)
import           Data.Text.Display       (Display (..))

import           GHC.Generics            (Generic)

import           UI.Attributes.Attribute

-- | An element id. Should be unique across the entire document.
--
-- __Example__
--
-- @
-- div [ id_ := "special-div" ]
-- @
--
-- would be matched by the CSS rule:
--
-- @
-- #special-div {
--   ...
-- }
-- @
newtype Id = Id Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (IsString, AsAttributeValue)

instance Display Id where
  displayBuilder (Id i) = "#" <> displayBuilder i
