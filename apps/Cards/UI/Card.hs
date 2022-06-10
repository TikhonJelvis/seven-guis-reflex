{-# LANGUAGE RecordWildCards #-}
module Cards.UI.Card where

import           Cards.Card         (Card (..), Rank (..), Suit (..), rankName,
                                     suitName)
import           Cards.Place        (Face (..))

import           Control.Monad      (void)

import           Data.Bool          (bool)
import           Data.Default.Class (Default, def)
import           Data.Functor       ((<&>))
import           Data.Map           (Map)
import           Data.Maybe         (fromMaybe, isJust)
import           Data.Text          (Text)
import qualified Data.Text          as Text

import           GHC.Generics       (Generic)

import           Linear             (V2 (..), V3 (..))

import           Reflex             (Dynamic, toggle)
import qualified Reflex.Dom         as Dom

import           Text.Printf        (printf)
import qualified Text.URI           as URI

import           UI.Attributes      (Transform (..), addClass, addTransform,
                                     removeClass, setClass, setProperty,
                                     toAttributes, with)
import qualified UI.Drag            as Drag
import           UI.Drag            (DragConfig (..), Drags (..))
import           UI.Element         (Dom, Html, elClass', elDynAttr')
import           UI.Main            (Runnable (..), withCss)
import           UI.Style           (Angle (..), BackfaceVisibility (..),
                                     backfaceVisibility, flipAround, rotate,
                                     scale, translate)
import           UI.SVG             (Command (..), GradientUnits (..),
                                     Rectangle (..), Stop (..), Svg, a, defs,
                                     g_, h, m, path, pattern_, radial,
                                     radialPath, rect, svgAttr', use, v)
import           UI.SVG.Attributes  (FillRule (..), Stroke (..), fill,
                                     paintWith, stroke)
import           UI.SVG.Haskell     (HaskellPaths (..), haskellPaths)
import           UI.Widget          (Enabled (..), button', image, label)

demo :: forall m t. Dom t m => m ()
demo = void $ elClass' "div" "card-demo" do
  label "Cards draggable across body"

  faceUp <- Reflex.toggle True =<< button' (pure "flip cards") (pure Enabled)
  let facing = bool FaceDown FaceUp <$> faceUp

  rec CardElement { drags } <-
        draggable (Card Ace Spades) facing
          def { attributes = dragAttributes drags }

  rec CardElement { drags } <-
        draggable (Card King Hearts) facing
          def { attributes = dragAttributes drags }

  pure ()
  where dragAttributes Drags { current } =
          Just $ whenDragged . isJust <$> current <*> pure []
        whenDragged True  =
          setProperty "z-index" "100" . rotate (Deg 5) . addClass "dragging"
        whenDragged False = removeClass "dragging"

-- | Various parameters that can be set up for a card element.
data CardConfig t = CardConfig
  { container  :: Maybe (Html t)
    -- ^ The container within which the card is draggable.
    --
    -- Drag events outside the container will not register.

  , dragging   :: Maybe (Dynamic t Enabled)
    -- ^ Is dragging enabled for the card?

  , attributes :: Maybe (Dynamic t (Map Text Text))
    -- ^ Any additional dynamic attributes.
    --
    -- Note that various card-specific properties will add to or
    -- override properties set here.
  }
  deriving stock (Generic)

-- | All values default to 'Nothing'
instance Default (CardConfig t) where

-- | Information about a card element.
data CardElement t = CardElement
  { drags :: Drags t
  , card  :: Card
  }
  deriving stock (Generic)

-- | Creates a draggable element representing a specific card.
--
-- The element will have the CSS class @card@ as well as classes for
-- its rank (@ace@, @two@... @ten@, @jack@, @queen@, @king@) and suit
-- (@clubs@, @diamonds@, @hearts@, @spades@).
--
-- __Examples__
--
-- An ace of spades that can be dragged around the body of the
-- document:
--
-- @
-- ace :: m (Element t)
-- ace = do
--   rec element <- card (Card Ace Spades) attributes
--       let attributes = translate <$> total <*> pure []
--       Drags { total } <- Drag.drags def element
--   pure element
-- @
draggable :: forall m t. Dom t m
          => Card
          -> Dynamic t Face
          -> CardConfig t
          -> m (CardElement t)
draggable card@Card { rank, suit } face CardConfig { container, dragging, attributes } = mdo
  let attributes' = do
        move <- translate <$> total
        draggingClass <- setClass "dragging" . isJust <$> current
        move . draggingClass . static <$> fromMaybe (pure []) attributes

  (element, _) <- elDynAttr' "div" attributes' do
    let facing = face <&> \case
          FaceUp   -> id
          FaceDown -> addTransform (Rotate3D (V3 0 1 0) (Deg 180))
    Dom.elDynAttr "div" (facing <*> pure [("class", "center")]) do
      front card (pure [])
      back haskellBack (pure [])

  drags@Drags { current, total } <-
    Drag.drags def { container, enabled = dragging } element

  pure CardElement { card, drags }
  where static =
          addClass "draggable" .
          addClass "card" .
          addClass (suitName suit) .
          addClass (rankName rank) .
          backfaceVisibility Hidden

-- * Card Fronts

-- | Create an HTML element for the front side of a specific card.
front :: forall m t. Dom t m
      => Card
      -- ^ The card this represents.
      -> Dynamic t (Map Text Text)
      -- ^ Additional attributes for the element.
      -> m (Html t)
front Card { rank, suit } attributes = do
  (element, _) <- elDynAttr' "div" (addClass "front" <$> attributes) do
    void $ image $ fromMaybe (error "Invalid URI!") $ URI.mkURI cardURI
  pure element
  where cardURI = "img/" <> rankName rank <> "-" <> suitName suit <> ".svg"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)

-- * Card Backs

-- | Create an HTML element for the back side of a card.
back :: forall a m t. Dom t m
     => m a
     -- ^ Design for the card back. Should be an element that can fill
     -- the space (example: 'haskellBack').
     -> Dynamic t (Map Text Text)
     -- ^ Additional attributes for the element.
     -> m (Html t, a)
back backDesign attributes = do
  elDynAttr' "div" (addClass "back" <$> attributes) backDesign

-- | A card back design based on the Haskell logo.
haskellBack :: forall m t. Dom t m => m (Svg t)
haskellBack = mdo
  fst <$> svgAttr' "svg" [("viewBox", "-100 -300 200 600")] do
    defs [ ("small-lambda", smallLambda)
         , ("lambda-tile", lambdaTile)
         , ("lambda-pattern", lambdaPattern)
         , ("background", backgroundGradient)
         ]

    -- background
    let background = with (fill $ paintWith "background") []
    rect (pure $ Rectangle "400" "600" "-200" "-300") (pure background)

    aroundLogo 400 600 110 $ pure $
      translate (V2 (-200) (-300)) $
      with EvenOdd $
      stroke "none" <> fill (paintWith "lambda-pattern")

    -- center logo
    g_ (pure $ scale 6 [])
      [ pair (translate $ V2 0 (-12))
      , pair (flipAround (Deg 90) . translate (V2 0 12))
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
          , logo $ pure $ translate (V2 17 0) $ flipAround (Deg 0) []
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
          , bottom (translate (V2 (-1) 14) [])
          ]
          where λ f = use "small-lambda" (pure $ f $ origin 8 6 [])
                top attrs = g_ (pure attrs)
                  [ λ id
                  , λ (flipAround (Deg 90) . translate (V2 8 0))
                  , λ (translate (V2 16 0))
                  ]
                bottom attrs = g_ (pure attrs)
                  [ λ (rotate (Deg 180))
                  , λ (flipAround (Deg 0) . translate (V2 8 0))
                  , λ (rotate (Deg 180) . translate (V2 16 0))
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

