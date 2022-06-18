module UI.SVG
  ( Svg

  , svgElement
  , svgElement'

  , svg
  , g

  , defs
  , Def (..)

  , use
  , pattern_

  , module UI.SVG.Attributes

  , circle
  , rect

  , module UI.SVG.Path
  , path

  , Stop (..)
  , linear
  , radial

  , svgAttribute
  , svgAttributes
  )
where

import           Control.Monad                     (forM)

import           Data.Functor                      ((<&>))
import           Data.Hashable                     (Hashable (..))
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           GHC.Generics                      (Generic)
import           GHC.TypeLits                      (KnownSymbol, symbolVal)

import           Reflex                            (Dynamic, Reflex)
import qualified Reflex.Dom                        as Dom

import           UI.Attributes                     (href, id_)
import           UI.Attributes.AttributeSet.Reflex (AttributeSet, toDom, (=:))
import           UI.Color                          (Color)
import           UI.Element                        (Dom, createElement)
import           UI.Element.IsElement              (FromElement (..),
                                                    IsElement (..))
import qualified UI.Event                          as Event
import           UI.Id                             (Id (..))
import           UI.SVG.Attributes
import           UI.SVG.Path
import qualified UI.Url                            as Url

-- * SVG Elements

newtype Svg t = Svg (Dom.Element Event.EventResult Dom.GhcjsDomSpace t)

instance FromElement Svg where
  type EventResult Svg = Event.EventResult
  fromElement = Svg

instance IsElement (Svg t) where
  rawElement (Svg e) = Dom._element_raw e

instance Reflex t => Dom.HasDomEvent t (Svg t) en where
  type DomEventType (Svg t) en = Event.EventResultType en
  domEvent eventName (Svg e) = Dom.domEvent eventName e

-- * Creating SVG Elements

-- | Create an SVG element.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- An @svg@ tag containing two circles:
--
-- @
-- svgElement @"svg" [] do
--   svgElement @"circle" [class_ =: "ball"] (pure ())
--   svgElement' @"circle" [class_ =: "ball"]
-- @
svgElement :: forall element a m t. (KnownSymbol element, Dom t m)
           => AttributeSet t element "SVG"
           -- ^ attributes
           -> m a
           -- ^ body
           -> m (Svg t, a)
svgElement attributes body = do
  (element, result) <- createElement Nothing tag (toDom attributes) body
  pure (Svg element, result)
  where tag = Text.pack $ symbolVal (Proxy :: Proxy element)
{-# INLINABLE svgElement #-}

-- | Create an SVG element with no body.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- svg' @"circle" [class_ =: "ball"]
-- @
svgElement' :: forall element m t. (KnownSymbol element, Dom t m)
            => AttributeSet t element "SVG"
            -> m (Svg t)
svgElement' attributes = fst <$> svgElement attributes (pure ())
{-# INLINABLE svgElement' #-}

-- ** Grouping

-- | A container for SVG elements that defines a new coordinate system
-- and viewport.
--
-- Can be used both inside other SVG elements and to embed SVG
-- elements into HTML.
svg :: forall a m t. Dom t m
    => AttributeSet t "svg" "SVG"
    -- ^ attributes
    -> m a
    -- ^ contents
    -> m (Svg t, a)
svg = svgElement

-- | A group of SVG elements.
g :: forall a m t. Dom t m
  => AttributeSet t "g" "SVG"
  -- ^ Attributes
  -> m a
  -- ^ SVG elements to group
  -> m (Svg t, a)
g = svgElement
{-# INLINE g #-}

-- | Define elements that are not rendered immediately, but can be
-- used in other parts of the SVG (ie with 'use').
--
-- __Examples__
--
-- Define and use an element:
--
-- @
-- do let c = Circle { center = V2 0 0, radius = 5 }
--    defs [ def "myElement" (circle (pure c)) [stroke =: "none"] ]
--    use "myElement" (pure [("x", "10"), ("y", "4")])
-- @
--
-- Define a pattern and use it to fill an element:
--
-- @
-- do let c = Circle { center = V2 0 0, radius = 10 }
--    defs [ def "bg" (circle $ pure c) [fill =: "#fff"] ]
--    rect (pure (Rectangle 100 100 0 0)) [fill =: paintWith "bg"]
-- @
defs :: forall a m t. Dom t m
     => [Def t m a]
     -- ^ A list of definitions.
     -> m (Map Id a)
defs definitions =
  snd <$> svgElement @"defs" [] do
    Map.fromList <$> forM definitions \ (Def name create base) -> do
      result <- create $ base <> [id_ =: name]
      pure (name, result)

-- | A definition combining an 'Id', a way to create elements and a
-- base set of attributes.
--
-- When this is used, the @id@ attribute ('id_') in the base
-- attributes will be overridden if set.
--
-- __Example__
--
-- @
-- defs
--   [ Def "example-circle" (circle (pure c)) [stroke =: "#000"]
--   , Def "example-gradient" linearGradient gradientAttrs
--   ]
-- @
data Def t m a where
  Def :: Id
      -> (AttributeSet t element "SVG" -> m a)
      -> AttributeSet t element "SVG"
      -> Def t m a

-- | Use an element defined elsewhere in the current document,
-- referenced by id.
--
-- __Examples__
--
-- Define and use an element:
--
-- @
-- do let c = Circle { center = V2 0 0, radius = 5 }
--    defs [ def "myElement" (circle (pure c)) [] ]
--    use "myElement" [ x =: 10, y =: 4 ]
-- @
use :: forall m t. Dom t m
    => Id
    -- ^ The id of the element to use
    -> AttributeSet t "use" "SVG"
    -- ^ Other attributes of the @use@ tag. An @href@ set here will be
    -- overridden.
    --
    -- Note that /most/ attributes will be the same as the element
    -- being used—only @x@, @y@, @width@ and @height@ will have any
    -- effect, but other attributes are still allowed.
    --
    -- See MDN:
    -- [use](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use)
    -> m (Svg t)
use elementId attributes = svgElement' $ attributes <> [ href =: Url.byId elementId ]

-- | A @pattern@ element defines an object that can be tiled along x-
-- and y-coordinate intervals.
--
-- __Example__
--
-- A rectangle tiled with white circles:
--
-- @
-- do
--   let c = circle (pure (Circle 0 10)) (pure $ fill "#fff")
--   defs [("bg", \ attrs -> pattern_ attrs c)]
--   rect (pure (Rectangle 100 100 0 0)) (pure $ fill $ paintWith "bg")
-- @
pattern_ :: forall a m t. Dom t m
         => AttributeSet t "pattern" "SVG"
         -- ^ Attributes
         -> m a
         -- ^ Pattern contents
         -> m (Svg t, a)
pattern_ = svgElement

-- ** Shapes

-- | Create a circle element with the given settings.
--
-- __Example__
--
-- A circle with a static center and radius:
--
-- @
-- circle [ cx =: 0
--        , cy =: 5
--        , r  =: 14
--        , stroke_width =: 4
--        , stroke =: "#3366ff"
--        ]
-- @
circle :: forall m t. Dom t m
       => AttributeSet t "circle" "SVG"
       -- ^ attributes
       -> m (Svg t)
circle = svgElement'

-- | Create a retangle (@rect@ element).
--
-- __Example__
--
-- A 10 × 10 rectangle at the origin:
--
-- @
-- rect [ x =: 0
--      , y =: 0
--      , width =: 10
--      , height =: 10
--      ]
-- @
rect :: forall m t. Dom t m
     => AttributeSet t "rect" "SVG"
     -- ^ attributes
     -> m (Svg t)
rect = svgElement'

-- | A @path@ element.
--
-- The shape of the path is specified by the @d@ attribute.
path :: forall m t. Dom t m
     => AttributeSet t "path" "SVG"
     -- ^ attributes
     -> m (Svg t)
path = svgElement'

-- ** Gradients

-- $ SVG supports __linear__ and __radial__ gradients.
--
-- Gradients are specified as a series of __stops__ which define the
-- colors to transition between along the length of the gradient.
--
-- A gradient is not rendered by itself; it needs to be used as the
-- @stroke@ or @fill@ property of another element. The best way to do
-- this is by defining gradients in a 'defs' block and using
-- 'paintWith':
--
-- @
-- do
--   let stops = [Stop 0 "#fff", Stop 0.5 "#000", Stop 1 "#fff"]
--
--   defs
--     [ Def "myLinear" (linear $ pure stops) []
--     , Def "myRadial" (radial $ pure stops) []
--     ]
--
--  let sample x_ y_ attributes = rect $ attributes <>
--        [ x =: px x_, y =: px y_, width =: px 100, height =: px 100 ]
--  sample 0 0 [ fill =: paintWith "myLinear" ]
--  sample 110 110 [ fill =: paintWith "myRadial" ]
-- @

-- | A gradient is made up of multiple __stops__ that specify the
-- color to transition part of the way through the gradient.
data Stop = Stop Double Color
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A linear gradient, transitioning between stops along a straight
-- line.
--
-- __Examples__
--
--
linear :: forall m t. Dom t m
       => Dynamic t [Stop]
       -- ^ Gradient stops
       -> AttributeSet t "linearGradient" "SVG"
       -- ^ Attributes
       -> m (Svg t)
linear stops attributes = fst <$> svgElement attributes (dynStops stops)

-- | A radial gradient which transitions colors from a center point
-- (@offset = 0@) out in a circle up to some radius (@offset = 1@).
radial :: forall m t. Dom t m
       => Dynamic t [Stop]
       -- ^ Gradient stops
       -> AttributeSet t "radialGradient" "SVG"
       -- ^ Attributes
       -> m (Svg t)
radial stops attributes = fst <$> svgElement attributes (dynStops stops)

-- | A gradient stop. Defines a color and a position that the gradient
-- will transition through.
stop :: forall m t. Dom t m
     => AttributeSet t "stop" "SVG"
     -- ^ attributes
     -> m (Svg t)
stop = svgElement'

-- | A dynamic set of @stop@ elements.
dynStops :: forall m t. Dom t m
         => Dynamic t [Stop]
         -> m ()
dynStops stops = Dom.dyn_ $ stops <&> mapM_ \ (Stop off color) ->
  stop [ stop_color =: color
       , offset =: off
       ]

-- * SVG Namespace

-- | The namespace for SVG elements: @http://www.w3.org/2000/svg@.
svgNamespace :: Text
svgNamespace = "http://www.w3.org/2000/svg"

-- | Wrap a text attribute name into an 'AttributeName' with the SVG
-- namespace (see 'svgNamespace').
svgAttribute :: Text -> Dom.AttributeName
svgAttribute = Dom.AttributeName (Just svgNamespace)

-- | Wrap a text attribute map into a map with 'AttributeName' keys
-- namespaced for SVG (see 'svgNamespace').
svgAttributes :: Map Text a -> Map Dom.AttributeName a
svgAttributes = Map.mapKeys svgAttribute
