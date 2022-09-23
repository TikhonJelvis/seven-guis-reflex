{-# LANGUAGE RecordWildCards #-}
-- | An extended example of using the SVG @path@ element to draw
-- variations on the Haskell logo.
module UI.Svg.Haskell where
import           Data.Default.Class                (Default (..))
import           Data.Hashable                     (Hashable)

import           GHC.Generics                      (Generic)

import           Linear                            (V2 (..))

import           UI.Attributes.AttributeSet.Reflex (AttributeSet, (=:))
import           UI.Element                        (Dom)
import           UI.Main                           (Runnable (..), withCss)
import           UI.Svg                            (Command (..), Path, Svg,
                                                    ViewBox (..), d, fill, g, h,
                                                    l, path, svg, v, viewBox)

-- * Haskell Logo in SVG

-- | Parameters for the Haskell logo.
--
-- The Haskell (@>λ=@) has three parts:
--
--  * a left angle bracket (@>@)
--  * a lambda (@λ@)
--  * an equals sign (@=@) cut off by the lambda
--
-- These settings control the dimensions of these parts and how they
-- relate to each other.
--
-- Defaults based on examples in Wiki—but the examples weren't all
-- identical, and the Haskell code in the Wiki seems to default to
-- different parameters from the SVG or TikZ code, if I understood it
-- correctly.
data Haskell = Haskell
  { lambdaWidth :: Double
    -- ^ Width of the > and λ strokes

  , equalsWidth :: Double
    -- ^ Width of the lines in the =

  , spacing     :: Double
    -- ^ Horizontal space between > and λ as well as λ and =

  , height      :: Double
    -- ^ Total height of logo.
    --
    -- The > and λ extend for the entire width of the logo.
    --
    -- The = is vertically centered within the logo, with @spacing@
    -- vertical space between the two lines.

  , width       :: Double
    -- ^ Total width of logo.
    --
    -- The width of the logo does not affect the width of the > and λ,
    -- which are always made up of diagonal lines at a 2/3 slope. The
    -- total width of the > and λ is only affected by @lineWidth@.
    --
    -- The = extends all the way to the righthand side of the logo.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Default Haskell where
  def = Haskell
    { lambdaWidth = 3
    , equalsWidth = 2
    , spacing     = 1
    , height      = 12
    , width       = 17
    }

-- | The four paths for each part of the Haskell logo.
data HaskellPaths = HaskellPaths
  { leftAngle  :: Path
  -- ^ The >
  , lambda     :: Path
  -- ^ The λ
  , topLine    :: Path
  -- ^ Top of the =
  , bottomLine :: Path
  -- ^ Bottom of the =
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | Generate SVG paths for a Haskell logo with the given settings.
haskellPaths :: Haskell -> HaskellPaths
haskellPaths Haskell {..} = HaskellPaths { leftAngle, lambda, topLine, bottomLine }
  where leftAngle =
          [ M 0 0
          , l halfX halfY
          , l (-halfX) halfY
          , h lambdaWidth
          , l halfX (-halfY)
          , l (-halfX) (-halfY)
          , Z
          ]

        lambda =
          [ M (lambdaWidth + spacing) 0
          , h lambdaWidth
          , l ((2/3) * height) height
          , h (-lambdaWidth)
          , l (-legX) (-legY)
          , l (-legX) legY
          , h (-lambdaWidth)
          , l halfX (-halfY)
          , Z
          ]

        topLine =
          [ M (cornerX topY) topY
          , l ((2/3) * equalsWidth) equalsWidth
          , H width
          , v (-equalsWidth)
          , Z
          ]

        bottomLine =
          [ M (cornerX bottomY) bottomY
          , l ((2/3) * equalsWidth) equalsWidth
          , H width
          , v (-equalsWidth)
          , Z
          ]

        halfX = (2/3) * halfY
        halfY = height / 2

        -- the bottom part of the lambda is 3/8 of the total height
        legY = (5/16) * height
        legX = (2/3) * legY

        -- the y coordinate of the /top/ of each line in the =
        topY = (height / 2) - (spacing / 2) - equalsWidth
        bottomY = (height / 2) + (spacing / 2)

        -- each line in the = is 1 unit away from the 2/3 diagonal of
        -- the λ, which makes the x position of their top-left corner
        -- a function of their y position (calculated above)
        cornerX topLeft = (2 * lambdaWidth) + (2 * spacing) + ((2/3) * topLeft)

-- | An SVG group (@g@ element) with paths for a Haskell logo.
--
-- Shape based on the SVG + Tikz versions of the logo from the Haskell
-- Wiki: https://wiki.haskell.org/TW-Logo-Haskell
--
-- Colors taken from Haskell.org logo
haskell :: forall m t. Dom t m
        => Haskell
        -> AttributeSet t
        -> m (Svg t)
haskell config attributes = fst <$> g attributes do
  path [ d =: leftAngle, fill =: "#453a62" ]
  path [ d =: lambda, fill =: "#5e5086" ]
  path [ d =: topLine, fill =: "#8f4e8b"]
  path [ d =: bottomLine, fill =: "#8f4e8b"]
  where HaskellPaths {..} = haskellPaths config

main :: IO ()
main = withCss "css/ui-demo.css" $ Runnable do
  svg [viewBox =: ViewBox (V2 0 0) (V2 340 340)] do
    haskell def []
