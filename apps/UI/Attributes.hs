{-# LANGUAGE FlexibleInstances #-}
module UI.Attributes where

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)
import qualified Data.Text as Text

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
