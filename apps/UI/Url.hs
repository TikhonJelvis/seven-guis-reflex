-- | A structured URL type.
module UI.Url
  ( Url (..)

  , toText
  , parseURL

  , byId
  )
where

import           Data.Hashable           (Hashable (..))
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..))
import qualified Data.Text.Lazy.Builder  as Builder

import           GHC.Generics            (Generic)

import qualified Text.URI                as URI
import           Text.URI                (URI)

import           UI.Attributes.Attribute (AsAttributeValue (..))
import           UI.Id                   (Id (..))

-- | A URL for referring to external locations like links,
-- images... etc.
--
-- Note: the @IsString@ instance is provided for convenience, but will
-- error at runtime if the URL does not parse. Alternatives:
--
--  * @'parseURL' :: Text -> Maybe URL@
--
--  * 'url' quasi-quoter for static checking (requires Template
--    Haskell)
newtype Url = Url { toURI :: URI }
  deriving stock (Show, Eq, Ord, Generic)

-- | Convert a 'Url' to text.
toText :: Url -> Text
toText = URI.render . toURI

-- | Tries to parse some text as a URL. Returns 'Nothing' if the URL
-- syntax is not valid.
parseURL :: Text -> Maybe Url
parseURL = fmap Url . URI.mkURI

-- | A 'Url' that references an element in the current document by its
-- id.
--
-- >>> toText $ byId "foo"
-- "#foo"
byId :: Id -> Url
byId (Id id_) = Url $ URI.emptyURI { URI.uriFragment = Just $ unsafe $ URI.mkFragment id_ }
  where unsafe = fromMaybe (error $ "Invalid id in Url: " <> Text.unpack id_)

instance Hashable Url where
  hashWithSalt salt = hashWithSalt salt . URI.render . toURI

instance Display Url where
  displayBuilder = Builder.fromText . URI.render . toURI

instance AsAttributeValue Url where
  toAttributeValue = URI.render . toURI
  fromAttributeValue = parseURL
