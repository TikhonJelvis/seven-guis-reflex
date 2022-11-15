module UI.Css.Parse
  ( parseMaybe

  , color
  , colorKeywords

  , number
  , percentage
  , numberOrPercentage
  )
where

import qualified Data.Colour.RGBSpace.HSL   as Colour
import qualified Data.Colour.SRGB           as Colour
import           Data.Functor               (void)
import           Data.HashMap.Strict        (HashMap, (!))
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import           Data.Void                  (Void)

import qualified Numeric

import           Text.Megaparsec            (Parsec, between, choice, many,
                                             notFollowedBy, parseMaybe, sepBy,
                                             try, (<?>), (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, hexDigitChar,
                                             space, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf                (printf)

import           UI.Color                   (Color (..))

type Parser = Parsec Void Text

-- * Values

-- $ Parsers for CSS values based on expressions.

-- | Parse a CSS color definition. CSS color expressions can take
-- several forms:
--
--   * RGB values in hex: @#XXX@ or @#XXXXXX@
--   * RGBA values in hex: @#XXXX@ or @#XXXXXXXX@
--   * Functions: @rgb()@, @rgba()@, @hsl()@, @hsla()@... etc
--   * Keywords: @blue@, @lightseagreen@... etc
--
-- >>> parseMaybe color "blue"
-- Just "#0000ff"
--
-- >>> parseMaybe color "orangered"
-- Just "#ff4500"
--
-- >>> parseMaybe color "#fff"
-- Just "#ffffff"
--
-- >>> parseMaybe color "#3366ff"
-- Just "#3366ff"
--
-- >>> parseMaybe color "#fff4"
-- Just "#ffffff44"
--
-- >>> parseMaybe color "#3366FFA4"
-- Just "#3366ffa4"
--
-- >>> UI.Color.toOpaque <$> parseMaybe color "#3366FFA4"
-- Just "#3366ff"
--
-- >>> parseMaybe color "rgb(0.2, 40%, 100%)"
-- Just "#3366ff"
--
-- >>> parseMaybe color "rgba(0.2, 40%, 100%, 1)"
-- Just "#3366ff"
--
-- >>> parseMaybe color "hsl(225, 100%, 60%)"
-- Just "#3366ff"
--
-- >>> parseMaybe color "hsla(225, 100%, 60%, 0.5)"
-- Just "#3366ff80"
color :: Parser Color
color =  named
     <|> try hex
     <|> try rgb
     <|> try rgba
     <|> try hsl
     <|> try hsla
  where named = do
          k <- choice (try . keyword <$> HashMap.keys colorKeywords) <?> "<named-color>"
          pure $ colorKeywords ! k

        hex = lexeme do
          char '#'
          digits <- many hexDigitChar
          convert digits

        -- TODO: better error handling for functions (rgb,
        -- rgba... etc)?

        rgb = do
          [r, g, b] <- function "rgb" numberOrPercentage
          pure $ Color { base = Colour.sRGB r g b, alpha = 1 }

        rgba = do
          [r, g, b, a] <- function "rgba" numberOrPercentage
          pure $ Color { base = Colour.sRGB r g b, alpha = a }

            -- TODO: deg/grad/rad units for hsl and hsla
        hsl = do
          [h, s, l] <- function "hsl" numberOrPercentage
          let Colour.RGB r g b = Colour.hsl h s l
          pure $ Color { base = Colour.sRGB r g b, alpha = 1 }

        hsla = do
          [h, s, l, a] <- function "hsla" numberOrPercentage
          let Colour.RGB r g b = Colour.hsl h s l
          pure $ Color { base = Colour.sRGB r g b, alpha = a }

            -- TODO: remaining CSS color functions

        convert = \case
          [r, g, b] ->
            convert [r, r, g, g, b, b]
          [r, g, b, a] ->
            convert [r, r, g, g, b, b, a, a]
          s@[_r1, _r2, _g1, _g2, _b1, _b2] ->
            pure $ Color { base = Colour.sRGB24read ('#':s), alpha = 1 }
          s@[_r1, _r2, _g1, _g2, _b1, _b2, a1, a2] ->
            pure $ Color
              { base  = Colour.sRGB24read ('#' : take 6 s)
              , alpha = toDouble $ readHex [a1, a2]
              }
          s -> fail $ printf "Invalid color hex digits: %s" s

        toDouble x = fromInteger x / 255
        readHex = fst . head . Numeric.readHex

-- | Definitions for each color keyword CSS supports.
colorKeywords :: HashMap Text Color
colorKeywords =
  [ ("black", "#000000")
  , ("silver", "#c0c0c0")
  , ("gray", "#808080")
  , ("white", "#ffffff")
  , ("maroon", "#800000")
  , ("red", "#ff0000")
  , ("purple", "#800080")
  , ("fuchsia", "#ff00ff")
  , ("green", "#008000")
  , ("lime", "#00ff00")
  , ("olive", "#808000")
  , ("yellow", "#ffff00")
  , ("navy", "#000080")
  , ("blue", "#0000ff")
  , ("teal", "#008080")
  , ("aqua", "#00ffff")
  , ("orange", "#ffa500")
  , ("aliceblue", "#f0f8ff")
  , ("antiquewhite", "#faebd7")
  , ("aquamarine", "#7fffd4")
  , ("azure", "#f0ffff")
  , ("beige", "#f5f5dc")
  , ("bisque", "#ffe4c4")
  , ("blanchedalmond", "#ffebcd")
  , ("blueviolet", "#8a2be2")
  , ("brown", "#a52a2a")
  , ("burlywood", "#deb887")
  , ("cadetblue", "#5f9ea0")
  , ("chartreuse", "#7fff00")
  , ("chocolate", "#d2691e")
  , ("coral", "#ff7f50")
  , ("cornflowerblue", "#6495ed")
  , ("cornsilk", "#fff8dc")
  , ("crimson", "#dc143c")
  , ("cyan", "#00ffff")
  , ("darkblue", "#00008b")
  , ("darkcyan", "#008b8b")
  , ("darkgoldenrod", "#b8860b")
  , ("darkgray", "#a9a9a9")
  , ("darkgreen", "#006400")
  , ("darkgrey", "#a9a9a9")
  , ("darkkhaki", "#bdb76b")
  , ("darkmagenta", "#8b008b")
  , ("darkolivegreen", "#556b2f")
  , ("darkorange", "#ff8c00")
  , ("darkorchid", "#9932cc")
  , ("darkred", "#8b0000")
  , ("darksalmon", "#e9967a")
  , ("darkseagreen", "#8fbc8f")
  , ("darkslateblue", "#483d8b")
  , ("darkslategray", "#2f4f4f")
  , ("darkslategrey", "#2f4f4f")
  , ("darkturquoise", "#00ced1")
  , ("darkviolet", "#9400d3")
  , ("deeppink", "#ff1493")
  , ("deepskyblue", "#00bfff")
  , ("dimgray", "#696969")
  , ("dimgrey", "#696969")
  , ("dodgerblue", "#1e90ff")
  , ("firebrick", "#b22222")
  , ("floralwhite", "#fffaf0")
  , ("forestgreen", "#228b22")
  , ("gainsboro", "#dcdcdc")
  , ("ghostwhite", "#f8f8ff")
  , ("gold", "#ffd700")
  , ("goldenrod", "#daa520")
  , ("greenyellow", "#adff2f")
  , ("grey", "#808080")
  , ("honeydew", "#f0fff0")
  , ("hotpink", "#ff69b4")
  , ("indianred", "#cd5c5c")
  , ("indigo", "#4b0082")
  , ("ivory", "#fffff0")
  , ("khaki", "#f0e68c")
  , ("lavender", "#e6e6fa")
  , ("lavenderblush", "#fff0f5")
  , ("lawngreen", "#7cfc00")
  , ("lemonchiffon", "#fffacd")
  , ("lightblue", "#add8e6")
  , ("lightcoral", "#f08080")
  , ("lightcyan", "#e0ffff")
  , ("lightgoldenrodyellow", "#fafad2")
  , ("lightgray", "#d3d3d3")
  , ("lightgreen", "#90ee90")
  , ("lightgrey", "#d3d3d3")
  , ("lightpink", "#ffb6c1")
  , ("lightsalmon", "#ffa07a")
  , ("lightseagreen", "#20b2aa")
  , ("lightskyblue", "#87cefa")
  , ("lightslategray", "#778899")
  , ("lightslategrey", "#778899")
  , ("lightsteelblue", "#b0c4de")
  , ("lightyellow", "#ffffe0")
  , ("limegreen", "#32cd32")
  , ("linen", "#faf0e6")
  , ("magenta", "#ff00ff")
  , ("mediumaquamarine", "#66cdaa")
  , ("mediumblue", "#0000cd")
  , ("mediumorchid", "#ba55d3")
  , ("mediumpurple", "#9370db")
  , ("mediumseagreen", "#3cb371")
  , ("mediumslateblue", "#7b68ee")
  , ("mediumspringgreen", "#00fa9a")
  , ("mediumturquoise", "#48d1cc")
  , ("mediumvioletred", "#c71585")
  , ("midnightblue", "#191970")
  , ("mintcream", "#f5fffa")
  , ("mistyrose", "#ffe4e1")
  , ("moccasin", "#ffe4b5")
  , ("navajowhite", "#ffdead")
  , ("oldlace", "#fdf5e6")
  , ("olivedrab", "#6b8e23")
  , ("orangered", "#ff4500")
  , ("orchid", "#da70d6")
  , ("palegoldenrod", "#eee8aa")
  , ("palegreen", "#98fb98")
  , ("paleturquoise", "#afeeee")
  , ("palevioletred", "#db7093")
  , ("papayawhip", "#ffefd5")
  , ("peachpuff", "#ffdab9")
  , ("peru", "#cd853f")
  , ("pink", "#ffc0cb")
  , ("plum", "#dda0dd")
  , ("powderblue", "#b0e0e6")
  , ("rosybrown", "#bc8f8f")
  , ("royalblue", "#4169e1")
  , ("saddlebrown", "#8b4513")
  , ("salmon", "#fa8072")
  , ("sandybrown", "#f4a460")
  , ("seagreen", "#2e8b57")
  , ("seashell", "#fff5ee")
  , ("sienna", "#a0522d")
  , ("skyblue", "#87ceeb")
  , ("slateblue", "#6a5acd")
  , ("slategray", "#708090")
  , ("slategrey", "#708090")
  , ("snow", "#fffafa")
  , ("springgreen", "#00ff7f")
  , ("steelblue", "#4682b4")
  , ("tan", "#d2b48c")
  , ("thistle", "#d8bfd8")
  , ("tomato", "#ff6347")
  , ("turquoise", "#40e0d0")
  , ("violet", "#ee82ee")
  , ("wheat", "#f5deb3")
  , ("whitesmoke", "#f5f5f5")
  , ("yellowgreen", "#9acd32")
  ]

  -- TODO: Does CSS allow spaces after a + or - sign?
-- | Parse a normal numeric literal.
--
-- >>> parseMaybe number "1"
-- Just 1.0
--
-- >>> parseMaybe number "1.0"
-- Just 1.0
--
-- >>> parseMaybe number "-1.0"
-- Just (-1.0)
number :: Parser Double
number = try float <|> integer
  where float = L.signed (pure ()) L.float
        integer = L.signed (pure ()) L.decimal

-- | Parse a percentage, converting to a double.
--
-- >>> parseMaybe percentage "100%"
-- Just 1.0
--
-- >>> parseMaybe percentage "0.1%"
-- Just 1.0e-3
--
-- >>> parseMaybe percentage "-0.1%"
-- Just (-1.0e-3)
percentage :: Parser Double
percentage = do
  n <- number
  char '%'
  pure (n / 100)

-- | Either a number or a percentage.
--
-- >>> parseMaybe numberOrPercentage "-0.1"
-- Just (-0.1)
--
-- >>> parseMaybe numberOrPercentage "0.1%"
-- Just 1.0e-3
numberOrPercentage :: Parser Double
numberOrPercentage = try percentage <|> number

-- * Parsing Utilities

-- | Parse a specific keyword.
keyword :: Text -> Parser Text
keyword word = lexeme (string word) <* notFollowedBy (alphaNumChar <|> char '_')

               -- TODO: alternate function syntax?
-- | Parse a function with the given name and type of argument.
--
-- >>> parseMaybe (function "rgb" numberOrPercentage) "rgb(1, -2.0, 3%)"
-- Just [1.0,-2.0,3.0e-2]
function :: Text
         -- ^ Function name (eg @rgb@, @hsl@)
         -> Parser a
         -> Parser [a]
function name argument = do
  keyword name
  between (symbol "(") (symbol ")") (argument `sepBy` symbol ",")


-- | Wraps a parser into a parser that ignores trailing whitespace and
-- comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser ()
symbol = void . L.symbol space
