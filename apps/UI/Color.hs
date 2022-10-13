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

import           UI.Attributes          (AsAttributeValue (..),
                                         CombineAttributeValue)

-- | A color with an alpha channel.
newtype Color = Color (AlphaColour Double)
  deriving stock (Eq, Show, Generic)

instance IsString Color where
  fromString css = case parseCSSColor $ Text.pack css of
    Just color -> color
    Nothing    -> error $ "Invalid color literal: " <> css

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

instance CombineAttributeValue Color
-- | Colors with 100% opacity are rendered as @#xxxxxx@ and colors
-- with any other level of opacity are rendered as @#xxxxxxxx@.
instance AsAttributeValue Color where
  toAttributeValue c = case toRgba c of
    (r, g, b, 0xff) -> Text.pack $ printf "#%.2x%.2x%.2x" r g b
    (r, g, b, a)    -> Text.pack $ printf "#%.2x%.2x%.2x%.2x" r g b a

  fromAttributeValue = parseCSSColor

instance Display Color where
  displayBuilder = Builder.fromText . toAttributeValue

    -- TODO: support other color formats
-- | Parse CSS color literals.
--
-- Currently only supports @#xxx@, @#xxxx@, @#xxxxxx@ and @#xxxxxxxx@
-- literals—covers any colors created through this library—but should
-- support /all/ valid CSS colors in the future.
parseCSSColor :: Text -> Maybe Color
parseCSSColor = go . Text.unpack
  where
    go = \case
      ['#', r, g, b] -> go ['#', r, r, g, g, b, b]
      ['#', r, g, b, a] -> go ['#', r, r, g, g, b, b, a, a]
      s@['#', _r1, _r2, _g1, _g2, _b1, _b2] ->
        Just $ fromColour $ Colour.sRGB24read s
      s@['#', _r1, _r2, _g1, _g2, _b1, _b2, a1, a2] ->
        Just $ Color $ Colour.withOpacity (base s) (toDouble $ readHex [a1, a2])
      _ -> Nothing

    base s = Colour.sRGB24read (take 7 s)
    readHex = fst . head . Numeric.readHex
    toDouble x = fromInteger x / 255

-- ** Opaque Colors

-- | An color without an alpha channel—always 100% opaque.
newtype Opaque = Opaque (Colour Double)
  deriving stock (Show, Eq, Generic)

-- | hash based on sRGB
instance Hashable Opaque where
  hashWithSalt s (Opaque c) = hashWithSalt s (r, g, b)
    where RGB r g b = Colour.toSRGB24 c

instance CombineAttributeValue Opaque

-- | Renders to @#xxxxxx@ format, always lowercase
--
-- >>> toAttributeValue (Opaque (Colour.black))
-- "#000000"
--
-- >>> toAttributeValue <$> fromAttributeValue @Opaque "#fFffFF"
-- Just "#ffffff"
--
-- See: 'parseOpaque'
instance AsAttributeValue Opaque where
  toAttributeValue (Opaque c) = Text.pack $ printf "#%.2x%.2x%.2x" r g b
    where Colour.RGB r g b = Colour.toSRGB24 c

  fromAttributeValue = parseOpaque

-- | Return the base color, ignoring its opacity.
--
-- This is semantically equivalent to @color `over` black@.
toOpaque :: Color -> Opaque
toOpaque (Color c) = Opaque $ c `over` Colour.black

-- | Convert an 'Opaque' to a 'Color' with α = 1.
fromOpaque :: Opaque -> Color
fromOpaque (Opaque c) = fromColour c

-- | Parse an opaque color.
--
-- This only supports the hex @#XXXXXX@ format (eg @#FFFFFF@ for
-- white) and is not case sensitive.
--
-- >>> parseOpaque "#FfFFff"
-- Just (Opaque (Data.Colour.SRGB.Linear.rgb 1.0 1.0 1.0))
--
-- >>> parseOpaque "#35f"
-- Nothing
--
-- >>> parseOpaque "#ffffff00"
-- Nothing
--
-- >>> parseOpaque "blue"
-- Nothing
parseOpaque :: Text -> Maybe Opaque
parseOpaque = go . Text.unpack
  where
    go = \case
      s@['#', _r1, _r2, _g1, _g2, _b1, _b2] ->
        Just $ Opaque $ Colour.sRGB24read s
      _ -> Nothing
