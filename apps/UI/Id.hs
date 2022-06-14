-- | Element ids—names that we can use to reference a specific HTML or
-- SVG element. Ids should be unique across a document.
module UI.Id
  ( Id (..)
  , Ids (..)
  )
where

import           Data.Hashable           (Hashable (..))
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..))
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector
import           Data.Vector.Instances   ()

import           GHC.Exts                (IsList (..))
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

-- | Multiple element ids. Used for attributes like 'for_'.
newtype Ids = Ids (Vector Id)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance IsList Ids where
  type Item Ids = Id
  fromList = Ids . fromList
  toList (Ids ids) = toList ids

    -- TODO: fromAttributeValue (once we've switched to megaparsec)
instance AsAttributeValue Ids where
  toAttributeValue (Ids xs) =
    Text.intercalate " " $ Vector.toList $ toAttributeValue <$> xs

  fromAttributeValue = undefined
