{-# LANGUAGE RecordWildCards #-}
-- | An extended example of using the SVG @path@ element to draw
-- variations on the Haskell logo.
module UI.SVG.Haskell where
import           Data.Hashable (Hashable)

import           GHC.Generics  (Generic)

import           UI.Element    (Dom)
import           UI.Main       (Runnable (..), withCss)
import           UI.SVG        (Command (..), Path, Svg, h, l, path, svgAttr',
                                v)

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

-- | Given a scale, these parameters match up the examples in the
-- Wiki, although the examples do not seem to be 100%
-- consistent. Using these parameters should give you something
-- indistinguishable from the "official" Haskell logo.
defaultHaskell :: Double
                  -- ^ Scaling factor, where 1 corresponds to @spacing
                  -- = 1@, making @lambdaWidth = 3@, @height = 12@ and
                  -- @width = 17@.
               -> Haskell
defaultHaskell α = Haskell
  { lambdaWidth = 3 * α
  , equalsWidth = 2 * α
  , spacing = α
  , height = 12 * α
  , width = 17 * α
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

-- | An SVG element with paths for a Haskell logo.
--
-- Shape based on the SVG + Tikz versions of the logo from the Haskell
-- Wiki: https://wiki.haskell.org/TW-Logo-Haskell
--
-- Colors taken from Haskell.org logo
haskell :: forall m t. Dom t m => Haskell -> m (Svg t)
haskell config = fst <$> svgAttr' "svg" [] do
  let HaskellPaths {..} = haskellPaths config
  path (pure leftAngle) (pure [("fill", "#453a62")])
  path (pure lambda) (pure [("fill", "#5e5086")])
  path (pure topLine) (pure [("fill", "#8f4e8b")])
  path (pure bottomLine) (pure [("fill", "#8f4e8b")])

main :: IO ()
main = withCss "css/ui-demo.css" (Runnable $ haskell $ defaultHaskell 5)
