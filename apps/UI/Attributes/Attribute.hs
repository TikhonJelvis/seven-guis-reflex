{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving  #-}
-- | The 'Attribute' type represents HTML/SVG/XML
-- attributes.
--
-- Attributes are structured in Haskell based on:
--
--  1. The type of values they take
--
--  2. What elements they apply to (either globally for HTML/SVG/etc,
--  or specific elements like @a@ and @area@)
--
-- __Examples__
--
-- The @class@ attribute, global across all HTML and SVG elements:
--
-- @
-- class_ :: Attribute ["HTML", "SVG"] (Set ClassName)
--                            ↑            ↑
--                         supports    type of values
-- @
--
-- Note: when a name overlaps with a reserved word or Prelude function
-- in Haskell (like @class@ or @id@), it will always be named with a
-- single trailing underscore (@class_@, @id_@).
--
-- The @placeholder@ attribute which only applies to HTML @input@
-- elements:
--
-- @
-- placeholder :: Attribute ["input"] Text
-- @
--
-- Note: this module is designed to be imported qualified. Common
-- pattern:
--
-- @
-- import           UI.Attributes.Attribute (Attribute)
-- import qualified UI.Attributes.Attribute as Attribute
-- @
module UI.Attributes.Attribute
  ( Attribute (Attribute)
  , name

  , supports
  , supports'

  , AsAttributeValue (..)
  , ShowRead (..)
  , Lowercase (..)

  , htmlWhitespace
  , isHtmlWhitespace
  )
where

import qualified Data.Char              as Char
import           Data.Hashable          (Hashable)
import           Data.Set               (Set)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Display      (Display (..))
import qualified Data.Text.Lazy.Builder as Builder

import           GHC.Generics           (Generic)
import           GHC.TypeLits           (Symbol)

import           Numeric.Natural        (Natural)

import           Text.Read              (readMaybe)

import           UI.Type.List           (KnownSymbols, knownSymbols)

-- * Attributes

-- $setup
-- >>> import Data.Text.Display (display)
-- >>> let class_ = Attribute @"class" @["HTML", "SVG"]
-- >>> let href = Attribute @"href" @["a", "area", "base", "link"]

-- | An attribute that can be set on HTML or XML elements.
--
-- The type of an attribute tracks the type of elements it can
-- support, either based on namespace (@"HTML"@, @"SVG"@) or based on
-- element names (@"a"@, @"area"@... etc).
--
-- __Examples__
--
-- An attribute that can be set on /any/ HTML or SVG element:
--
-- @
-- class_ :: Attribute ["HTML", "SVG"] (Set ClassName)
-- class_ = Attribute "class"
-- @
--
-- Note: when a name overlaps with a reserved word or Prelude function
-- in Haskell (like @class@ or @id@), it will always be named with a
-- single trailing underscore (@class_@, @id_@).
--
-- An attribute that can be set on HTML @input@ elements:
--
-- @
-- placeholder :: Attribute ["input"] Text
-- placeholder = Attribute "placeholder"
-- @
newtype Attribute (supports :: [Symbol]) a = Attribute { name :: Text }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | A Haskell literal with an explicit type annotation:
--
-- >>> show class_
-- "(Attribute \"class\" :: Attribute \"class\" '[\"HTML\",\"SVG\"])"
instance KnownSymbols supports => Show (Attribute supports a) where
  show attribute@Attribute { name } =
    "(Attribute " <> show name <> " :: Attribute '" <> show (supports attribute) <> ")"

-- | Just the attribute name
--
-- >>> display class_
-- "class"
instance Display (Attribute supports a) where
  displayBuilder = Builder.fromText . name

-- | Get the elements an attribute supports.
--
-- >>> supports class_
-- ["HTML","SVG"]
--
-- >>> supports href
-- ["a","area","base","link"]
supports :: forall supports a. KnownSymbols supports
         => Attribute supports a -> [Text]
supports _ = supports' @supports

-- | Get the elements an attribute supports.
--
-- This has an ambiguous type variable, so it need to be used with an
-- explicit type application:
--
-- >>> supports' @["SVG", "HTML"]
-- ["SVG","HTML"]
supports' :: forall (supports :: [Symbol]). KnownSymbols supports => [Text]
supports' = knownSymbols @supports

-- * Attribute Values

-- | Types that can be converted to and from HTML or XML attribute
-- values.
class AsAttributeValue a where
  -- TODO: handle escaping attribute values properly?
  -- | Convert to an attribute value.
  --
  -- This should be be valid HTML syntax for an attribute value
  -- /without/ surrounding quotes, but with any internal quotes
  -- escaped as necessary.
  toAttributeValue :: a -> Text

  -- | Parse HTML syntax for an attribute into the corresponding
  -- value. Returns 'Nothing' if the value cannot be parsed, either
  -- because the HTML is invalid or because the corresponding Haskell
  -- type does not cover all valid possibilities.
  fromAttributeValue :: Text -> Maybe a

  -- | How to combine values when multiple values are specified for
  -- the same attribute.
  --
  -- By default, this will keep the last value. The following two 'a'
  -- elements behave identically:
  --
  -- @
  -- a [href := "foo", href := "bar"] "bar"
  -- a [href := "bar"] "bar"
  -- @
  --
  -- However, some attributes like 'class_' have logical ways to
  -- combine multiple values. The following two 'div' elements have
  -- the same classes:
  --
  -- @
  -- div [class_ := ["foo"], class_ := ["bar"]]
  -- div [class_ := ["foo", "bar"]]
  -- @
  --
  -- This is particularly useful when we want to set some classes
  -- statically and other classes dynamically:
  --
  -- @
  -- div [ class_ := ["draggable", "card"]
  --     , class_ :== classIf "dragged" <$> beingDragged
  --     ]
  -- @
  combineAttributeValues :: a -> a -> a
  combineAttributeValues _old new = new

instance AsAttributeValue Text where
  toAttributeValue = id
  fromAttributeValue = Just

instance AsAttributeValue String where
  toAttributeValue = Text.pack
  fromAttributeValue = Just . Text.unpack

deriving via ShowRead Int instance AsAttributeValue Int
deriving via ShowRead Integer instance AsAttributeValue Integer
deriving via ShowRead Word instance AsAttributeValue Word
deriving via ShowRead Natural instance AsAttributeValue Natural
deriving via ShowRead Double instance AsAttributeValue Double

-- *** Deriving Via

-- | A type of deriving 'AsAttributeValue' for types that have 'Show'
-- and 'Read' instances that work as attribute values directly.
--
-- __Example__
--
-- A number type for x-axis coordinates specifically:
--
-- @
-- newtype X = X Double
--   deriving newtype (Show, Read)
--   deriving AsAttributeValue via (ShowRead X)
-- @
--
-- Note that @deriving stock (Show, Read)@ or @deriving (Show, Read)@
-- /would not work/ because the 'Show' and 'Read' instances would
-- involve the @X@ constructor!
newtype ShowRead a = ShowRead a
  deriving newtype (Show, Read)

instance (Show a, Read a) => AsAttributeValue (ShowRead a) where
  toAttributeValue = Text.pack . show
  fromAttributeValue = readMaybe . Text.unpack

-- | A type for deriving 'AsAttributeValue' by lowercasing the first
-- character of the type's 'show' function.
--
-- __Example__
--
-- @
-- data Spread = Pad | Reflect | Repeat
--   deriving stock (Show, Read)
--   deriving AsAttributeValue via (Lowercase Spread)
-- @
newtype Lowercase a = Lowercase a
  deriving newtype (Show, Read)

instance (Show a, Read a) => AsAttributeValue (Lowercase a) where
  toAttributeValue = Text.toLower . Text.pack . show
  fromAttributeValue = readMaybe . Text.unpack . capitalize
    where capitalize t = case Text.uncons t of
            Nothing      -> ""
            Just (c, cs) -> Text.cons (Char.toUpper c) cs

-- * Attribute Lexical Syntax

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
