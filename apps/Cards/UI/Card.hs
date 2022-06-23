{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cards.UI.Card where

import           Cards.Card                        (Card (..), Rank (..),
                                                    Suit (..), rankName,
                                                    suitName)
import           Cards.Place                       (Face (..))

import           Control.Monad                     (void)

import           Data.Bool                         (bool)
import           Data.Default.Class                (Default, def)
import           Data.Functor                      ((<&>))
import           Data.Maybe                        (fromMaybe, isJust)

import           GHC.Generics                      (Generic)

import           Linear                            (V2 (..), V3 (..))

import           Reflex                            (Dynamic, toggle)

import           UI.Attributes                     (class_, style)
import           UI.Attributes.AttributeSet.Reflex (AttributeSet, (=:), (==:))
import           UI.Class                          (ClassName (..), classIf)
import qualified UI.Css                            as Css
import           UI.Css                            (Angle (..), Transform (..),
                                                    u)
import qualified UI.Css.Transforms                 as Transforms
import qualified UI.Drag                           as Drag
import           UI.Drag                           (DragConfig (..), Drags (..))
import qualified UI.Element                        as Element
import           UI.Element                        (Dom)
import qualified UI.Html                           as Html
import           UI.Html                           (Html)
import           UI.Html.Attributes                (src)
import           UI.Main                           (Runnable (..), withCss)
import qualified UI.SVG                            as Svg
import           UI.SVG                            (Command (..), Def (..), Svg,
                                                    a, d, h, m, v, viewBox)
import           UI.SVG.Attributes                 (GradientUnits (..),
                                                    ViewBox (..), cx, cy, fill,
                                                    fill_rule, gradientUnits,
                                                    height, paintWith, r,
                                                    stroke, stroke_width,
                                                    transform, transform_origin,
                                                    width, x, y)
import           UI.SVG.Haskell                    (HaskellPaths (..),
                                                    haskellPaths)
import qualified UI.Url                            as Url

demo :: forall m t. Dom t m => m ()
demo = void $ Html.div_ [ class_ =: ["card-demo"] ] do
  label "Cards draggable across body"

  pressed <- snd <$> Html.button' "flip cards" []
  faceUp <- Reflex.toggle True pressed
  let facing = bool FaceDown FaceUp <$> faceUp

  rec CardElement { drags } <-
        draggable (Card Ace Spades) facing
          def { attributes = dragAttributes drags }

  rec CardElement { drags } <-
        draggable (Card King Hearts) facing
          def { attributes = dragAttributes drags }

  pure ()
  where dragAttributes :: Drags t -> Maybe (AttributeSet t "div" "HTML")
        dragAttributes Drags { current } = Just
          [ class_ ==: classIf "dragging" . isJust <$> current
          , style  ==: whenDragged . isJust <$> current
          ]

        whenDragged True =
          Css.setProperty "z-index" (100 :: Double) $ Transforms.rotate (Deg 5) mempty
        whenDragged False = []

        label = Html.div_ [ class_ =: ["label"] ] . Element.text

-- | Various parameters that can be set up for a card element.
data CardConfig t = CardConfig
  { container       :: Maybe (Html t)
    -- ^ The container within which the card is draggable.
    --
    -- Drag events outside the container will not register.

  , draggingEnabled :: Maybe (Dynamic t Bool)
    -- ^ Is dragging enabled for the card?

  , attributes      :: Maybe (AttributeSet t "div" "HTML")
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
draggable
  card@Card { rank, suit }
  face
  CardConfig { container, draggingEnabled, attributes } = mdo
  let attributes' = fromMaybe [] attributes <>
        [ style ==: Transforms.translate <$> total <*> pure mempty
        , style =: Css.backfaceVisibility Css.Hidden mempty
        , class_ ==: classIf "dragging" . isJust <$> current
        , class_ =: ["draggable", "card", ClassName $ suitName suit, ClassName $ rankName rank]
        ]

  (element, _) <- Html.div_ attributes' do
    let facing = face <&> \case
          FaceUp   -> mempty
          FaceDown ->
            Transforms.addTransform (Rotate3D (V3 0 1 0) (Deg 180)) mempty
    Html.div_ [ class_ =: ["center"], style ==: facing ] do
      front card []
      back haskellBack []

  drags@Drags { current, total } <-
    Drag.drags def { container, enabled = draggingEnabled } element

  pure CardElement { card, drags }

-- * Card Fronts

-- | Create an HTML element for the front side of a specific card.
front :: forall m t. Dom t m
      => Card
      -- ^ The card this represents.
      -> AttributeSet t "div" "HTML"
      -- ^ Additional attributes for the element.
      -> m (Html t)
front Card { rank, suit } attributes = do
  (element, _) <- Html.div_ ([ class_ =: ["front"] ] <> attributes) do
    void $ Html.img [ src =: cardUrl ]
  pure element
  where cardUrl = fromMaybe (error "Invalid card URL!") $ Url.parseUrl $
          "img/" <> rankName rank <> "-" <> suitName suit <> ".svg"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)

-- * Card Backs

-- | Create an HTML element for the back side of a card.
back :: forall a m t. Dom t m
     => m a
     -- ^ Design for the card back. Should be an element that can fill
     -- the space (example: 'haskellBack').
     -> AttributeSet t "div" "HTML"
     -- ^ Additional attributes for the element.
     -> m (Html t, a)
back backDesign attributes =
  Html.div_ ([class_ =: ["back"]] <> attributes) backDesign

-- | A card back design based on the Haskell logo.
haskellBack :: forall m t. Dom t m => m (Svg t)
haskellBack = mdo
  fst <$> Svg.svg [ viewBox =: ViewBox (V2 (-100) (-300)) (V2 200 600) ] do
    Svg.defs
      [ Def "small-lambda" smallLambda []
      , Def "lambda-tile" lambdaTile []
      , Def "lambda-pattern" lambdaPattern []
      , Def "background" backgroundGradient []
      ]

    -- background
    Svg.rect
      [ x      =: "-200"
      , y      =: "-300"
      , width  =: "400"
      , height =: "600"
      , fill   =: paintWith "background"
      ]

    aroundLogo 400 600 110
      [ fill_rule =: Svg.Evenodd
      , stroke    =: "none"
      , fill      =: paintWith "lambda-pattern"
      , transform =: Svg.translate (V2 (-200) (-300))
      ]

    -- center logo
    Svg.g [ transform =: Svg.scale 6 ] do
      pair [ transform =: Svg.translate (V2 0 (-12)) ]
      pair [ transform =: Svg.flipAround (Deg 90) <> Svg.translate (V2 0 12) ]

  where backgroundGradient = Svg.radial (pure stops) . (<> attributes)
          where attributes =
                  [ cx            =: "0"
                  , cy            =: "0"
                  , r             =: "400"
                  , gradientUnits =: UserSpaceOnUse
                  ]
                stops =
                  [ Svg.Stop 0    "#000000"
                  , Svg.Stop 0.25 "#251a42"
                  , Svg.Stop 0.5  "#453a62"
                  , Svg.Stop 0.75 "#5e5086"
                  , Svg.Stop 1    "#8f4e8b"
                  ]

        -- WebKitGTK was not handling transform-origin or %
        -- measurments for dimensions correctly, so the code has some
        -- hardcoded numbers to work around that
        --
        -- each logo is 17 units wide and 12 units tall, everything
        -- else is based on that

        pair attributes = Svg.g attributes do
          logo []
          logo [ transform =: Svg.translate (V2 17 0) <> Svg.flipAround (Deg 0) ]

        logo :: AttributeSet t "g" "SVG" -> m ()
        logo attributes = void $ Svg.g (base <> attributes) do
          Svg.path $ part <> [ fill =: "#fff7", d =: leftAngle ]
          Svg.path $ part <> [ fill =: "#fff6", d =: lambda ]
          Svg.path $ part <> [ fill =: "#fff5", d =: topLine ]
          Svg.path $ part <> [ fill =: "#fff5", d =: bottomLine ]
          where part = [ stroke =: "#fff", stroke_width =: u 0.4 ]
                base = [ transform        =: Svg.rotate (Deg 20)
                       , transform_origin =: Transforms.origin (V2 8.5 6)
                       ]

        -- a path /around/ the logo in the center—a rectangle with a
        -- circle of the given radius removed from the center
        --
        -- using the EvenOdd fill rule will fill the rectangle but not
        -- the circle in the middle
        aroundLogo width height radius attributes =
          Svg.path $ attributes <>
            [ d =:
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
            ]

        smallLambda attributes = Svg.path $ attributes <> [ d =: lambda ]
          -- with (def { color = "#fff", width = 0.75 }) .
          -- with (fill $ paintWith "background") <$> attributes

        -- a quadrant of lambdas facing up and down to be tiled:
        --
        -- ↑↓
        -- ↓↑
        lambdaTile attributes = fst <$> Svg.g attributes do
          top []
          bottom [ transform =: Svg.translate (V2 (-1) 14) ]
          where top attributes = Svg.g attributes do
                  λ []
                  λ [ transform =: Svg.flipAround (Deg 90) <> Svg.translate (V2 8 0) ]
                  λ [ transform =: Svg.translate (V2 16 0) ]
                bottom attributes = Svg.g attributes do
                  λ [ transform =: Svg.rotate (Deg 180) ]
                  λ [ transform =: Svg.flipAround (Deg 0) <> Svg.translate (V2 8 0) ]
                  λ [ transform =: Svg.rotate (Deg 180) <> Svg.translate (V2 16 0) ]

                λ attributes = Svg.use "small-lambda" $
                  attributes <> [ transform_origin =: Transforms.origin (V2 8 6)
                                , style =: [("transform-box", "fill-box")]
                                ]

        lambdaPattern attributes =
          fst <$> Svg.pattern_ (base <> attributes) (lambdaTile [])
          where base = [ viewBox =: ViewBox (V2 6 0) (V2 16 28)
                       , width   =: "12%"
                       , height  =: "14%"
                       ]

        HaskellPaths { leftAngle, topLine, bottomLine, lambda } = haskellPaths def
