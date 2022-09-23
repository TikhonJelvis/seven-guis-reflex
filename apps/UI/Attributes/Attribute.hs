{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving  #-}
-- | The 'Attribute' type represents HTML/SVG/XML
-- attributes.
--
-- An attribute tracks:
--
--   1. The name of the attribute
--   2. The type of values the attribute supports
--   3. How to map from values to the DOM
--   4. How to combine multiple instances of the same attribute
--
-- If you need to set an attribute with a non-standard or dynamically
-- generated name, you can use 'textAttribute' or 'Attribute''s
-- 'IsString' instance:
--
-- @
-- [ textAttribute "data-foo" =: "some text" ]
-- [ "data-foo" =: "some-text" ]
-- @
--
-- __Examples__
--
-- The @class@ attribute, global across all HTML and SVG elements:
--
-- @
-- class_ :: Attribute (Set ClassName)
-- @
--
-- Note: when a name overlaps with a reserved word or Prelude function
-- in Haskell (like @class@ or @id@), it will always be named with a
-- single trailing underscore (@class_@, @id_@).
--
-- An element can have any number of classes where order and
-- duplicates do not matter, so we model values as sets of class names
-- that are combined with set union. This is useful for mixing static
-- and dynamic classes:
--
-- @
-- div [ class_ := ["draggable", "card"]
--     , class_ :== classIf "dragged" <$> beingDragged
--     ]
-- @
--
-- 'Attribute' values don't have to map 1:1 to actual attributes on
-- the element. We can also define __logical__ attributes that map to
-- any number of physical attributes. For example, we could define a
-- @p@ attribute that expands to @x@ and @y@:
--
-- @
-- p :: Attribute Color
-- p = logical "p" \ V2 x y ->
--   ["x" =. x, "y" =. y]
-- @
module UI.Attributes.Attribute
  ( Attribute
  , name
  , type_
  , toAttributes
  , combine

  , native
  , logical
  , boolean
  , (=.)
  , override

  , AsAttributeValue (..)
  , ShowRead (..)
  , Lowercase (..)

  , htmlWhitespace
  , isHtmlWhitespace
  , skipHtmlWhitespace
  , htmlSpaceList
  , htmlCommaList
  )
where

import           Control.Applicative          ((<|>))

import qualified Data.Char                    as Char
import           Data.GADT.Compare            (GCompare (..), GEq (..),
                                               GOrdering (..))
import           Data.GADT.Show               (GShow (..))
import           Data.Map                     (Map)
import           Data.Proxy                   (Proxy (..))
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Display            (Display (..), display)
import qualified Data.Text.Lazy.Builder       as Builder
import           Data.Time                    (Day, LocalTime, TimeOfDay)
import qualified Data.Time.Format.ISO8601     as Time
import           Data.Typeable                (TypeRep, Typeable,
                                               type (:~:) (..), typeRep)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector

import           GHC.Generics                 (Generic)

import           Numeric.Natural              (Natural)

import           Text.ParserCombinators.ReadP (ReadP, many1, satisfy, sepBy,
                                               skipMany, string)
import           Text.Read                    (readMaybe)

import qualified Unsafe.Coerce                as Unsafe

-- * Attributes

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
-- class_ :: Attribute (Set ClassName)
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
-- placeholder :: Attribute Text
-- placeholder = native
-- @
--
-- A logical attribute that maps to several concrete attributes:
--
-- @
-- c :: Attribute Circle
-- c = logical \ Circle { center = V2 x y, radius } ->
--   ["cx" =. x, "cy" =. y, "r" =. radius]
-- @
data Attribute a = Attribute
  { toAttributes :: a -> Map Text Text
  -- ^ How to convert the attribute value to one or more HTML
  -- attributes.

  , combine      :: a -> a -> a
  -- ^ How to combine two instances of this attribute.
  --
  -- Default is that the second value overrides the first value.

  , name         :: Text
  -- ^ A user-readable name for the attribute.
  --
  -- For native attributes, this will be the same as the element's
  -- attribute name. For logical attributes, this can be anything, but
  -- should be useful for error messages.

  , type_        :: Maybe TypeRep
  -- ^ A runtime representation of the value type in the attribute,
  -- used to keep attribute set operations safe.
  --
  -- Attributes set with 'textAttribute' will have the 'typeRep_'
  -- 'Nothing'—conceptually, those are set to the "native" value
  -- directly rather than going through a Haskell type. In practice,
  -- this lets us treat 'textAttribute' specially vs "normal"
  -- attributes with type 'Text' so that the overriding behavior of
  -- 'textAttribute' is consistent.
  }
  deriving stock (Generic)

-- | Just the attribute name
instance Display (Attribute a) where
  displayBuilder = Builder.fromText . name

instance Show (Attribute a) where show = Text.unpack . display

-- instances we need to use attributes as keys to DMaps

instance GShow Attribute where gshowsPrec = showsPrec

instance GEq Attribute where
  geq a b
    | type_ a == type_ b && name a == name b =
      Just $ Unsafe.unsafeCoerce Refl
    | otherwise =
      Nothing
      -- as long as (type_ a) matches the value type of a—which the
      -- smart constructors for Attribute ensure—this is safe
      --
      -- there is presumably a way to do this without unsafeCoerce,
      -- but I do not know what it is


-- | When attributes have the same name, the /lower/ attribute
-- according to this ordering will take priority
--
-- In particular, this means that if the same name has a key with
-- @type_ = Nothing@ and a key with @type_ = Just ...@, the version
-- with @Nothing@ will take precedence.
--
-- When the same name has /multiple/ @type_ = Just typeRep@ keys the
-- ordering is unspecified—currently depends on the 'Ord' instance for
-- 'SomeTypeRep'.
instance GCompare Attribute where
  gcompare a b
    | Just Refl <- geq a b = GEQ
    | isLessThan           = GLT
    | otherwise            = GGT
    where isLessThan
            | name a == name b = type_ a < type_ b
            | otherwise        = name a < name b

-- | Define a __native__ attribute: an 'Attribute' that corresponds 1:1
-- with an attribute on the element.
native :: forall a. (Typeable a, AsAttributeValue a)
       => Text
       -- ^ attribute name
       -> Attribute a
native name = Attribute
  { toAttributes = \ a -> [(name, toAttributeValue a)]
  , combine      = combineAttributeValues
  , type_        = Just $ typeRep (Proxy @a)
  , name
  }

-- | Define a __logical__ attribute by providing a mapping from the
-- attribute value to a set of HTML/XML attribute-value pairs.
--
-- Values will be combined by taking the newer value, overriding the
-- older value.
--
-- __Example__
--
-- @
-- c :: Attribute Circle
-- c = logical "c" \ Circle { center = V2 x y, radius } ->
--       ["cx" =. x, "cy" =. y, "r" =. radius]
-- @
logical :: forall a. Typeable a
        => Text
        -- ^ attribute name
        -> (a -> Map Text Text)
        -- ^ mapping to native attributes
        -> Attribute a
logical name toAttributes = Attribute
  { toAttributes
  , combine = const
  , name
  , type_ = Just $ typeRep (Proxy @a)
  }

-- | A boolean attribute. 'True' means the attribute is present on the
-- element, 'False' means it isn't.
--
-- __Example__
--
-- The @checked@ attribute for checkboxes:
--
-- @
-- checked :: Attribute Bool
-- checked = boolean "checked"
-- @
--
-- With this, we have:
--
-- @
-- checkbox [ checked =: True ] never
-- -- ⇒ <input type="checkbox" checked>
--
-- checkbox [ checked =: False ] never
-- -- ⇒ <input type="checkbox">
-- @
boolean :: Text -> Attribute Bool
boolean name = logical name $ \case
  True  -> [(name, "")]
  False -> []

-- | Encode a pair of an attribute name and value.
--
-- This is a helper function for defining logical attributes:
--
-- @
-- c :: Attribute Circle
-- c = logical "c" \ Circle { center = V2 x y, radius } ->
--       ["cx" =. x, "cy" =. y, "r" =. radius]
-- @
--
-- is the same as:
--
-- @
-- c :: Attribute Circle
-- c = logical "c" \ Circle { center = V2 x y, radius } ->
--       [ ("cx", toAttributeValue x)
--       , ("cy", toAttributeValue y)
--       , ("r", toAttributeValue radius)
--       ]
-- @
(=.) :: AsAttributeValue a => Text -> a -> (Text, Text)
attribute =. value = (attribute, toAttributeValue value)

-- | Override any value set for the given name, using the provided
-- 'Text' value directly on the element.
--
-- This lets us:
--
--  * override attributes like @class_@ that collect multiple values
--
--  * set attributes to invalid/non-standard/unsupported values
--
--  * set attributes not supported by the library or on elements the
--    library does not support
--
--  * set attributes where the attribute name is dynamic
--
-- A value set with 'override' will always take precdence over other
-- native/logical attributes with the same name. If 'override' is used
-- multiple times, only the /last/ value will be set.
--
-- __Example__
--
-- Setting a @data-<foo>@ attribute with a dynamic name:
--
-- @
-- data_ :: Text -> Attribute Text
-- data_ name = override ("data-" <> name)
--
-- myElement dataValue = div [ data_ "my-data" =: dataValue ] (pure ())
-- @
--
-- Overriding the @type@ of an @input@ element, normally set through
-- the type parameter:
--
-- @
-- -- will be a password field, not a text field
-- myInput = input @"text" [ override "type" =: "password" ]
-- @
--
-- Set a non-standard (Safari-only) attribute:
--
-- @
-- input @"text" [ override "autocorrect" =: "off" ]
-- @
override :: Text -> Attribute Text
override name = Attribute
  { name
  , type_        = Nothing -- different than "normal" Text attribute
  , toAttributes = \ value -> [(name, value)]
  , combine      = const
  }

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

-- | For optional values. When nothing, convert to HTML as the empty
-- string (@""@).
--
-- Note: for @Text@, this means that @Just ""@ and @Nothing@ are not
-- distinguishable as attribute values.
instance AsAttributeValue a => AsAttributeValue (Maybe a) where
  toAttributeValue = maybe "" toAttributeValue
  fromAttributeValue ""   = Just Nothing
  fromAttributeValue text = Just <$> fromAttributeValue text

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
deriving via ShowRead Day instance AsAttributeValue Day

instance AsAttributeValue TimeOfDay where
  toAttributeValue = Text.pack . Time.iso8601Show
  fromAttributeValue (Text.unpack -> str) =
    Time.iso8601ParseM str <|> Time.iso8601ParseM (str <> ":00")
    -- seconds are optional in the normalized HTML time format

instance AsAttributeValue LocalTime where
  toAttributeValue = Text.pack . Time.iso8601Show
  fromAttributeValue (Text.unpack -> str) =
    Time.iso8601ParseM str <|> Time.iso8601ParseM (str <> ":00")
    -- the normalized datetime-local format in HTML leaves out the
    -- seconds when they are :00

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

instance (Show a) => Display (Lowercase a) where
  displayBuilder = Builder.fromText . Text.toLower . Text.pack .show

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
-- | Skips any number of HTML whitespace characters.
skipHtmlWhitespace :: ReadP ()
skipHtmlWhitespace = skipMany $ satisfy isHtmlWhitespace

-- | Parses zero or more whitespace-separate values.
htmlSpaceList :: ReadP a -> ReadP (Vector a)
htmlSpaceList value =
  Vector.fromList <$> sepBy value (many1 $ satisfy isHtmlWhitespace)

-- | Parses zero or more comma-separated values.
htmlCommaList :: ReadP a -> ReadP (Vector a)
htmlCommaList value =
  Vector.fromList <$> sepBy value (skipHtmlWhitespace *> string "," *> skipHtmlWhitespace)
