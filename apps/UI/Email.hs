-- | Well-formed (ie validated) email addresses.
module UI.Email
  ( Email

  , validate
  , isValid

  , toText
  )
where

import           Data.Hashable           (Hashable (..))
import           Data.Maybe              (isJust)
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..))
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy.Builder  as Builder
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           GHC.Generics            (Generic)

import qualified Text.Email.Validate     as Validate
import qualified Text.Printf             as Text

import           UI.Attributes.Attribute (AsAttributeValue (..),
                                          isHtmlWhitespace)

-- | A valid email address with its parts encoded in UTF-8.
newtype Email = Email Validate.EmailAddress
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Email where
  hashWithSalt s = hashWithSalt s . toText

instance Display Email where
  displayBuilder = Builder.fromText . toText

-- | runtime error if address format is invalid
instance IsString Email where
  fromString string = case validate $ Text.pack string of
    Just email -> email
    Nothing    -> error $ Text.printf "Invalid email syntax: “%s”" string

instance AsAttributeValue Email where
  toAttributeValue = toText
  fromAttributeValue = validate

-- | comma separated list of email addresses
instance AsAttributeValue (Vector Email) where
  toAttributeValue =
    Text.intercalate "," . map toAttributeValue . Vector.toList
  fromAttributeValue text = Vector.fromList <$>
    mapM validate (filter (Text.all isHtmlWhitespace) $ Text.split (== ',') text)

-- | Validate an email address.
--
-- __Examples__
--
-- >>> validate "tikhon@example.com"
-- Just (Email "tikhon@example.com")
--
-- >>> validate "tikhon+something   @ example . com"
-- Just (Email "tikhon+something@example.com")
--
-- >>> validate "not.good"
-- Nothing
validate :: Text -> Maybe Email
validate text = Email <$> Validate.emailAddress (Text.encodeUtf8 text)

-- | Is the given email address syntactically valid?
--
-- __Examples__
--
-- >>> isValid "tikhon@example.com"
-- True
--
-- >>> isValid "tikhon+something   @ example . com"
-- True
--
-- >>> isValid "not.good"
-- False
isValid :: Text -> Bool
isValid = isJust . validate

-- | Convert an email address to text.
--
-- The address will not contain spaces or comments.
--
-- __Examples__
--
-- >>> toText <$> validate "tikhon@example.com"
-- Right "tikhon@example.com"
--
-- >>> toText <$> validate "tikhon+something   @ example . com"
-- Right "tikhon+something@example.com"
toText :: Email -> Text
toText (Email e) = Text.decodeUtf8 $ Validate.toByteString e
