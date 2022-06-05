module UI.Color where

import           Data.Colour            (AlphaColour, Colour, over)
import qualified Data.Colour            as Colour
import           Data.Colour.SRGB       (RGB (..))
import qualified Data.Colour.SRGB       as Colour
import           Data.Hashable          (Hashable (..))
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Display      (Display (..))
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Word              (Word8)

import           GHC.Generics           (Generic)

import qualified Numeric

import           Text.Printf            (printf)

import           UI.Attributes          (ToAttributeValue (..))

-- *** Color

-- | A CSS color.
newtype Color = Color (AlphaColour Double)
  deriving stock (Eq, Show, Generic)

instance IsString Color where
  fromString ['#', r, g, b] =
    fromString ['#', r, r, g, g, b, b]
  fromString ['#', r, g, b, a] =
    fromString ['#', r, r, g, g, b, b, a, a]
  fromString s@['#', _r1, _r2, _g1, _g2, _b1, _b2] =
    fromColour $ Colour.sRGB24read s
  fromString s@['#', _r1, _r2, _g1, _g2, _b1, _b2, a1, a2] =
    let base = Colour.sRGB24read (take 7 s) in
    Color $ Colour.withOpacity base (toDouble $ readHex [a1, a2])
    where readHex = fst . head . Numeric.readHex
          toDouble x = fromInteger x / 255
  fromString invalid = error $ "Invalid color: " <> invalid

-- | hash based on sRGB + α-channel
instance Hashable Color where
  hashWithSalt s (Color c) = hashWithSalt s (r, g, b, α)
    where α = Colour.alphaChannel c
          RGB r g b = Colour.toSRGB24 (c `over` Colour.black)

-- | Create a 'Color' that is a fully opaque version of the given
-- 'Colour'.
fromColour :: Colour Double -> Color
fromColour = Color . Colour.opaque

-- | Break a 'Color' out into its r, g, b and a channels
-- respectively. Each channel takes on a value 0–255.
toRgba :: Color -> (Word8, Word8, Word8, Word8)
toRgba (Color c) = (r, g, b, a)
  where Colour.RGB r g b = Colour.toSRGB24 (c `over` Colour.black)
        a = round $ 255 * Colour.alphaChannel c

-- | Convert a 'Color' to its CSS representation.
--
-- Colors with 100% opacity are rendered as @#xxxxxx@ and colors with
-- any other level of opacity are rendered as @#xxxxxxxx@.
toCss :: Color -> Text
toCss c = case toRgba c of
  (r, g, b, 0xff) -> Text.pack $ printf "#%.2x%.2x%.2x" r g b
  (r, g, b, a)    -> Text.pack $ printf "#%.2x%.2x%.2x%.2x" r g b a

instance ToAttributeValue Color where
  toAttributeValue = toCss

instance Display Color where
  displayBuilder = Builder.fromText . toCss
