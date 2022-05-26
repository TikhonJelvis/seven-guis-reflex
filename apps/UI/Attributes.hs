{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Attributes where

import           Control.Lens  (both)

import           Data.Bool     (bool)
import qualified Data.Foldable as Foldable
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Text     (Text)
import qualified Data.Text     as Text

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

-- ** CSS Classes

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

-- ** Style

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
