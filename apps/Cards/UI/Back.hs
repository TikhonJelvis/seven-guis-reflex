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
import           UI.Style           (Angle (..), flipAround, rotate, scale,
                                     translate)
import           UI.SVG             (GradientUnits (..), Rectangle (..),
                                     Stop (..), defs, g_, path, radial,
                                     radialPath, rect, svgAttr')
import           UI.SVG.Attributes  (Stroke (..), fill, paintWith)
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
    defs [ ("lambda", path (pure lambda) . fmap (with $ fill "#fff"))
         , ("background", backgroundGradient)
         ]

    -- background
    let background = with (fill $ paintWith "background") []
    rect (pure $ Rectangle "400" "600" "-200" "-300") (pure background)

    -- center logo
    g_ (pure $ scale 6 [])
      [ pair (translate $ Point 0 (-12))
      , pair (flipAround (Deg 90) . translate (Point 0 12))
      ]

  where backgroundGradient =
          radial (pure stops) .
          fmap (with (radialPath 0 400) . with UserSpaceOnUse)
          where stops =
                  [ Stop 0 "#000000"
                  , Stop 0.7 "#453a62"
                  , Stop 1 "#5e5086"
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
          [ path (pure leftAngle)  (logoPart "#fff")
          , path (pure lambda)     (logoPart "#fff")
          , path (pure topLine)    (logoPart "#fff")
          , path (pure bottomLine) (logoPart "#fff")
          ]
        HaskellPaths {..} = haskellPaths def

        logoPart color = pure $
          fill "none" <> toAttributes def { width = 0.25, color }

        origin :: Double -> Double -> Map Text Text -> Map Text Text
        origin x y =
          setProperty "transform-origin" (Text.pack $ printf "%fpx %fpx" x y) .
          setProperty "transform-box" "fill-box"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
