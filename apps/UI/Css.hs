{-# LANGUAGE UndecidableInstances #-}
module UI.Css where

import           Data.Hashable           (Hashable)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text

import           GHC.Exts                (IsList)
import           GHC.Generics            (Generic)

import           UI.Attributes.Attribute (AsAttributeValue (..))

-- TODO: structured representation of CSS rules?
-- | A set of CSS rules for a single element.
--
-- When the attribute is set multiple times, the map of rules is
-- combined. If the same /rule/ ends up being set multiple times, the
-- newer definition takes precendence.
newtype CssRules = CssRules (Map Text Text)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (IsList, Semigroup, Monoid)

instance AsAttributeValue CssRules where
  toAttributeValue (CssRules (Map.toList -> rules)) =
    Text.intercalate ";" [ property <> " : " <> value | (property, value) <- rules ]

    -- TODO: support CSS parsing!
  fromAttributeValue = error "CSS parsing not supported yet"

  combineAttributeValues = (<>)
