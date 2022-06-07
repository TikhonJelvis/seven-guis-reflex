{-# LANGUAGE RecordWildCards #-}
-- | Functions for rendering (hopefully) nice-looking card backs.
module Cards.UI.Back where

import           Control.Monad      (void)

import           Data.Default.Class (def)
import           Data.Map           (Map)
import           Data.Maybe         (isJust)
import           Data.Text          (Text)
import qualified Data.Text          as Text

import           Reflex             (Dynamic)
import qualified Reflex.Dom         as Dom

import           Text.Printf        (printf)

import           UI.Attributes      (ToAttributes (toAttributes), addClass,
                                     setClass, setProperty, with)
import qualified UI.Drag            as Drag
import           UI.Drag            (DragConfig (..), Drags (..))
import           UI.Element         (Dom, Html, elClass', elDynAttr')
import           UI.Main            (Runnable (..), withCss)
import           UI.Point           (Point (..))
import           UI.Style           (Angle (..), flipAround, px, rotate, scale,
                                     translate)
import           UI.SVG             (Command (..), GradientUnits (..),
                                     Rectangle (..), Stop (..), a, defs, g_, h,
                                     m, path, pattern_, radial, radialPath,
                                     rect, svgAttr', use, v)
import           UI.SVG.Attributes  (FillRule (..), Stroke (..), fill,
                                     paintWith, stroke)
import           UI.SVG.Haskell     (HaskellPaths (..), haskellPaths)
import           UI.Widget          (Enabled (..), label)

demo :: forall m t. Dom t m => m ()
demo = mdo
  void $ elClass' "div" "card-demo" mdo
    label "Draggable face-down card."
    void $ back Nothing (pure Enabled) (pure []) haskellBack

-- | Create a draggable face-down card.
back :: forall m t. Dom t m
     => Maybe (Html t)
     -- ^ Dragging container
     -> Dynamic t Enabled
     -- ^ Is dragging the card enabled?
     -> Dynamic t (Map Text Text)
     -- ^ Dynamic attributes
     -> m ()
     -- ^ The cardback design (probably an image or svg)
     -> m (Drags t)
back container dragging attributes backDesign = mdo
  (element, _) <- elDynAttr' "div" attributes' do
    Dom.elClass "div" "back-design" backDesign

  drags@Drags { current, total } <-
    Drag.drags def { container, enabled = Just dragging } element
  let attributes' = do
        move <- translate <$> total
        draggingClass <- setClass "dragging" . isJust <$> current
        move . draggingClass . classes <$> attributes

  pure drags
  where classes =
          addClass "draggable" .
          addClass "card" .
          addClass "back"

-- | A card back design based on the Haskell logo.
haskellBack :: forall m t. Dom t m => m ()
haskellBack = mdo
  void $ svgAttr' "svg" [("viewBox", "-100 -300 200 600")] do
    defs [ ("small-lambda", smallLambda)
         , ("lambda-tile", lambdaTile)
         , ("lambda-pattern", lambdaPattern)
         , ("background", backgroundGradient)
         ]

    -- background
    let background = with (fill $ paintWith "background") []
    rect (pure $ Rectangle "400" "600" "-200" "-300") (pure background)

    aroundLogo 400 600 110 $ pure $
      translate (Point (-200) (-300)) $
      with EvenOdd $
      stroke "none" <> fill (paintWith "lambda-pattern")

    -- center logo
    g_ (pure $ scale 6 [])
      [ pair (translate $ Point 0 (-12))
      , pair (flipAround (Deg 90) . translate (Point 0 12))
      ]

  where backgroundGradient =
          radial (pure stops) .
          fmap (with (radialPath 0 400) . with UserSpaceOnUse)
          where stops =
                  [ Stop 0    "#000000"
                  , Stop 0.25 "#251a42"
                  , Stop 0.5  "#453a62"
                  , Stop 0.75 "#5e5086"
                  , Stop 1    "#8f4e8b"
                  ]

        -- WebKitGTK was not handling transform-origin or %
        -- measurments for dimensions correctly, so the code has some
        -- hardcoded numbers to work around that
        --
        -- each logo is 17 units wide and 12 units tall, everything
        -- else is based on that

        pair f = g_ (pure $ f [])
          [ logo (pure [])
          , logo $ pure $ translate (Point 17 0) $ flipAround (Deg 0) []
          ]

        logo attributes = g_ (rotate (Deg 20) . origin 8.5 6 <$> attributes)
          [ path (pure leftAngle)  $ logoPart (fill "#fff7")
          , path (pure lambda)     $ logoPart (fill "#fff6")
          , path (pure topLine)    $ logoPart (fill "#fff5")
          , path (pure bottomLine) $ logoPart (fill "#fff5")
          ]
        HaskellPaths {..} = haskellPaths def

        logoPart attrs = pure $ attrs <> toAttributes def { width = 0.4, color = "#fff" }

        -- a path /around/ the logo in the center—a rectangle with a
        -- circle of the given radius removed from the center
        --
        -- using the EvenOdd fill rule will fill the rectangle but not
        -- the circle in the middle
        aroundLogo width height radius = path $ pure
          [ M 0 0
          , v height
          , h width
          , v (-height)
          , h (-width)

          , M (width / 2) (height / 2)
          , m radius 0
          , a radius radius 0 True True (-2 * radius) 0
          , a radius radius 0 True True (2 * radius) 0
          , Z
          ]

        smallLambda attributes = path (pure lambda) $
          with (def { color = "#fff", width = 0.75 }) .
          with (fill $ paintWith "background") <$> attributes

        -- a quadrant of lambdas facing up and down to be tiled:
        --
        -- ↑↓
        -- ↓↑
        lambdaTile attributes = g_ attributes
          [ top []
          , bottom (translate (Point (-1) 14) [])
          ]
          where λ f = use "small-lambda" (pure $ f $ origin 8 6 [])
                top attrs = g_ (pure attrs)
                  [ λ id
                  , λ (flipAround (Deg 90) . translate (Point 8 0))
                  , λ (translate (Point 16 0))
                  ]
                bottom attrs = g_ (pure attrs)
                  [ λ (rotate (Deg 180) . translate (Point 0 0))
                  , λ (flipAround (Deg 0) . translate (Point 8 0))
                  , λ (rotate (Deg 180) . translate (Point 16 0))
                  ]

        lambdaPattern attributes =
          fst <$> pattern_ attributes' (lambdaTile $ pure [])
          where attributes' = (base <>) <$> attributes
                base = [ ("viewBox", "6 0 16 28")
                       , ("width", "12%")
                       , ("height", "14%")
                       ]

        origin :: Double -> Double -> Map Text Text -> Map Text Text
        origin x y =
          setProperty "transform-origin" (Text.pack $ printf "%fpx %fpx" x y) .
          setProperty "transform-box" "fill-box"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
