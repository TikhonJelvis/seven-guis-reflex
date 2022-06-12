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
--                            ↑               ↑
--                         supports     type of values
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
-- 'Attribute' values don't have to map 1:1 to actual attributes on
-- the element. We can also define __logical__ attributes that map to
-- any number of physical attributes. For example, we could define a
-- @p@ attribute that expands to @x@ and @y@:
--
-- @
-- p :: Attribute ["p"] Color
-- p = logical "p" \ V2 x y ->
--   ["x" =. x, "y" =. y]
-- @
module UI.Attributes.Attribute
  ( Attribute
  , name
  , type_
  , supports
  , toAttributes

  , native
  , logical
  , (=.)

  , AsAttributeValue (..)
  , ShowRead (..)
  , Lowercase (..)

  , htmlWhitespace
  , isHtmlWhitespace
  , skipHtmlWhitespace
  )
where

import qualified Data.Char                    as Char
import           Data.Map                     (Map)
import           Data.Proxy                   (Proxy (..))
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Display            (Display (..))
import qualified Data.Text.Lazy.Builder       as Builder
import           Data.Typeable                (TypeRep, Typeable, typeRep)

import           GHC.Generics                 (Generic)
import           GHC.TypeLits                 (Symbol)

import           Numeric.Natural              (Natural)

import           Text.ParserCombinators.ReadP (ReadP, skipMany, satisfy)
import           Text.Read                    (readMaybe)

import           UI.Type.List                 (KnownSymbols, knownSymbols)

-- * Attributes

-- $setup
-- >>> import Data.Text.Display (display)
-- >>> let class_ = native "class" @["HTML", "SVG"] @ClassName
-- >>> let href = native "href" @["a", "area", "base", "link"] @Url

-- | An attribute that can be set on HTML or XML elements.
--
-- This type supports two kinds of attributes:
--
--  * __native__ attributes that correspond 1:1 to element attributes
--    (example: @class@, @id@, @width@)
--
--  * __logical__ attributes that map to /different/ attributes on the
--    element (example: @c@ that expands to @cx@, @cy@ and @r@ for
--    circles)
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
-- class_ = native
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
-- placeholder = native
-- @
--
-- A logical attribute that maps to several concrete attributes:
--
-- @
-- c :: Attribute ["circle"] Circle
-- c = logical \ Circle { center = V2 x y, radius } ->
--   ["cx" =. x, "cy" =. y, "r" =. radius]
-- @
data Attribute (supports :: [Symbol]) a = Attribute
  { toAttributes :: a -> Map Text Text
  -- ^ How to convert the attribute value to one or more HTML
  -- attributes.

  , name         :: Text
  -- ^ A user-readable name for the attribute.
  --
  -- For native attributes, this will be the same as the element's
  -- attribute name. For logical attributes, this can be anything, but
  -- should be useful for error messages.

  , type_        :: TypeRep
  -- ^ A runtime representation of the value type in the attribute,
  -- used to keep attribute set operations safe.
  }
  deriving stock (Generic)

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
supports _ = knownSymbols @supports

-- | Define a __native__ attribute: an 'Attribute' that corresponds 1:1
-- with an attribute on the element.
native :: forall supports a. (Typeable a, AsAttributeValue a)
       => Text
       -- ^ attribute name
       -> Attribute supports a
native name = Attribute
  { toAttributes = \ a -> [(name, toAttributeValue a)]
  , type_ = typeRep (Proxy @a)
  , name
  }

-- | Define a __logical__ attribute by providing a mapping from the
-- attribute value to a set of HTML/XML attribute-value pairs.
--
-- __Example__
--
-- @
-- c :: Attribute '["circle"] Circle
-- c = logical "c" \ Circle { center = V2 x y, radius } ->
--       ["cx" =. x, "cy" =. y, "r" =. radius]
-- @
logical :: forall supports a. Typeable a
        => Text
        -- ^ attribute name
        -> (a -> Map Text Text)
        -- ^ mapping to native attributes
        -> Attribute supports a
logical name toAttributes =
  Attribute { toAttributes, name, type_ = typeRep (Proxy @a) }

-- | Encode a pair of an attribute name and value.
--
-- This is a helper function for defining logical attributes:
--
-- @
-- c :: Attribute '["circle"] Circle
-- c = logical "c" \ Circle { center = V2 x y, radius } ->
--       ["cx" =. x, "cy" =. y, "r" =. radius]
-- @
--
-- is the same as:
--
-- @
-- c :: Attribute '["circle"] Circle
-- c = logical "c" \ Circle { center = V2 x y, radius } ->
--       [ ("cx", toAttributeValue x)
--       , ("cy", toAttributeValue y)
--       , ("r", toAttributeValue radius)
--       ]
-- @
(=.) :: AsAttributeValue a => Text -> a -> (Text, Text)
attribute =. value = (attribute, toAttributeValue value)

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

-- ** Deriving Via

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

                 -- TODO: use a real parser combinator library?
-- | A parser that skips any number of HTML whitespace characters.
skipHtmlWhitespace :: ReadP ()
skipHtmlWhitespace = skipMany $ satisfy isHtmlWhitespace
