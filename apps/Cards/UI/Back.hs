-- | Functions for rendering (hopefully) nice-looking card backs.
module Cards.UI.Back where

import           Control.Monad      (void)

import           Data.Default.Class (def)
import           Data.Map           (Map)
import           Data.Maybe         (isJust)
import           Data.Text          (Text)

import qualified Reflex
import           Reflex             (Dynamic, Reflex (Event))
import qualified Reflex.Dom         as Dom

import           UI.Attributes      (addClass, addTransform, setClass,
                                     setProperty)
import qualified UI.Drag            as Drag
import           UI.Drag            (DragConfig (..), Drags (..))
import           UI.Element         (Dom, Html, elClass', elDynAttr')
import           UI.Main            (Runnable (..), withCss)
import           UI.Point           (Point (..))
import           UI.Style           (Angle (..), Transform (..), addTransform,
                                     flipAround, matrix, px, rotate, scale,
                                     translate)
import           UI.SVG             (Circle (..), Rectangle (..), circle, g_,
                                     rect, svgAttr')
import           UI.SVG.Haskell     (haskell)
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
  void $ svgAttr' "svg" container do
    centerLogo (pure [])
    rect (pure Rectangle { height = "100%", width = px 1, x = "50%", y = "0" }) (pure [])
    rect (pure Rectangle { height = px 1, width = "100%", x = "0", y = "50%" }) (pure [])
  where centerLogo attributes = void $ svgAttr' "svg" centerAttrs do
          pair attributes
          pair attributes

        pair attributes = g_ (transformSettings <$> attributes)
          [ logo $ translate' "0" "-50%" <$> attributes
          , logo $ flipAround (Deg 0) . translate' "-100%" "-50%" <$> attributes
          ]

        logo attributes = haskell def $
          rotate (Deg 20) . transformSettings <$> attributes

        centerAttrs =
          [ ("viewBox", "-100 -30 200 60")
          , ("height", "120px")
          , ("width", "340px")
          , ("x", "0")
          , ("y", "0")
          ]

        translate' x y = addTransform (Translate x y "0")

        container =
          setProperty "height" "100%" $
          setProperty "width" "100%"
          []

        transformSettings =
          setProperty "transform-origin" "center" .
          setProperty "transform-box" "fill-box"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
