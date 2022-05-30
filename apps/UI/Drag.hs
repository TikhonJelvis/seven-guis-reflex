{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module UI.Drag where

import           Control.Applicative             (liftA2)
import           Control.Monad                   (void)

import qualified Data.ByteString                 as BS
import           Data.Default.Class              (Default (..))
import           Data.Functor                    ((<&>))
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)

import qualified GHCJS.DOM.CSSStyleDeclaration   as Style
import qualified GHCJS.DOM.Document              as Document
import qualified GHCJS.DOM.ElementCSSInlineStyle as Element

import qualified Language.Javascript.JSaddle     as Js

import qualified Reflex
import           Reflex                          (Dynamic, Event)
import qualified Reflex.Dom                      as Dom

import           UI.Attributes                   (Angle (..), Transition (..),
                                                  rotate, s, scale, transition,
                                                  translate)
import           UI.Element                      (Dom, Element, elClass',
                                                  elDynAttr')
import           UI.Event                        (Modifier (Shift),
                                                  MouseButton (..),
                                                  MouseEventResult (..), button,
                                                  client, mouseEvent, on,
                                                  performJs)
import           UI.Point                        (Point (..), distance)
import           UI.Style                        (getComputedProperty,
                                                  setProperty)
import           UI.Widget                       (label, ul)

import qualified Witherable

demo :: forall m t. (Dom t m) => m ()
demo = void do
  ul (pure [("class", "drag-demo")])
    [ example "Follow cursor exactly" def translate
    , example "Horizontal only" def xOnly
    , example "Vertical only" def yOnly
    , example "Snap to 50px grid" def (snapTo 50)
    , example "Rotate" def rotateByDistance
    , example "Scale" def scaleByDistance
    , example "Middle mouse button while holding @shift@" shiftConfig translate

    , snapBack
    ]
  dragAnywhere
  where xOnly Point { x } = translate (Point x 0)
        yOnly Point { y } = translate (Point 0 y)

        snapTo n Point { x, y } = translate (Point (toGrid n x) (toGrid n y))
        toGrid n x = x - fromInteger (round x `mod` n)

        rotateByDistance p = rotate (Turn $ distance p 0 / 100)

        scaleByDistance p = scale (1 + distance p 0 / 250)

        shiftConfig = def
          { mouseEventFilter = \ e ->
              (button e == Auxiliary) && (Shift `elem` modifiers e ) }

        dragAnywhere = mdo
          (element, _) <- elDynAttr' "div" attributes $
            Dom.text "drag me!"
          let attributes = translate <$> total <*> pure [("class", "drag-me")]
          Drags { total } <- drags def element
          pure ()

        example description config doDrag = mdo
          label description
          (container, _) <- elClass' "div" "drag-example" mdo
            (element, _) <- elDynAttr' "div" attributes (pure ())
            let attributes = doDrag <$> total <*> pure []
            Drags { total } <- drags config { container = Just container } element
            pure ()
          pure ()

        snapBack = mdo
          label "Snap back after each drag"
          (container, _) <- elClass' "div" "drag-example" mdo
            (element, _) <- elDynAttr' "div" attributes (pure ())
            let attributes = dragOrSnap <$> current <*> base
            Drags { current, start, end } <- drags def { container = Just container } element

            -- TODO: some design that makes this behavior less
            -- convoluted?
            computed <- Reflex.performEvent $
              getComputedProperty element "transform" <$ start
            transform <- Reflex.holdDyn Nothing $
              Reflex.leftmost [computed, Nothing <$ end]
            let base = transform <&> \case
                  Just t  -> setProperty "transform" t []
                  Nothing -> []
            pure ()
          pure ()

        dragOrSnap = \case
          Just d  -> translate d
          Nothing -> transition snap . translate (Point 0 0)

        snap = def { property = "transform", duration = s 1
 }

-- | Information about how a user interacts with an element by
-- dragging.
data Drags t = Drags
  { current  :: Dynamic t (Maybe Point)
    -- ^ The x and y distance moved by the mouse during the currently
    -- active drag. 'Nothing' when the user is not dragging this
    -- element.
    --
    -- If you just need to know wether an element is currently being
    -- dragged, you can use @isJust@:
    --
    -- @
    -- isDragged = isJust <$> current
    -- @

  , finished :: Dynamic t Point
    -- ^ The total x and y distance moved by the mouse during /already
    -- finished/ drags. If the user is /currently/ dragging the
    -- element, the movement from that is not included.

  , total    :: Dynamic t Point
    -- ^ The total x and y distance moved by the mouse counting /both/
    -- 'finished' and, if applicable, 'current'.

  , start    :: Event t Point
    -- ^ The user started dragging the element at the given client X
    -- and Y coordinates.

  , end      :: Event t Point
    -- ^ The user stopped dragging the element at the given client X
    -- and Y coordinates.
  }
             -- TODO: Does it make more sense for start and end to
             -- have the distance moved, just like the behaviors? If
             -- so, what distance: current or total?

-- | Configure how to measure drags for an item.
data DragConfig d t = DragConfig
  { container        :: Maybe (Element t)
  -- ^ Restrict the dragging to a container. Events outside the
  -- container do not count for the drag distance and don't end a
  -- drag.
  --
  -- The default is 'Nothing', which will use events from the entire
  -- document body.

  , mouseEventFilter :: MouseEventResult -> Bool
  -- ^ A function to specify which mouse events can /start/ drags.
  --
  -- By default, this restricts to the main (usually left) mouse
  -- button:
  --
  -- @
  -- \ e -> button e == Main
  -- @
  --
  -- __Example__:
  --
  -- An interaction that only works when the user shift-clicks with
  -- the auxiliary (middle) mouse button:
  --
  -- @
  -- let mouseEventFilter e =
  --   (button e == Auxiliary) && (Shift `elem` modifiers e)
  -- in drags def { mouseEventFilter } draggableElement
  -- @
  }

instance Default (DragConfig d t) where
  def = DragConfig
    { container        = Nothing
    , mouseEventFilter = \ e -> button e == Main
    }

-- | Keeps track of how a user drags an element.
--
-- A "drag" starts when the user holds the main mouse button over the
-- element and ends when the user releases the mouse button.
--
-- The element itself does not have to move; this only provides the
-- /inputs/ from mouse drag actions. This gives you flexibility for
-- how to respond to the drag event: have the element follow the
-- cursor exactly, restrict to one dimension, snap to a 10px
-- grid... etc.
--
-- __Examples__
--
-- Set up a draggable element with a @doDrag@ that controls how the
-- element actuall drags:
--
-- @
-- draggable = do
--   rec let attributes = doDrag <$> total <*> pure []
--       (element, ()) <- elDynAttr' "div" attributes (pure ())
--       Drags { total } <- drags container element
--   pure ()
-- @
--
-- Follow the cursor exactly:
--
-- @
-- doDrag = translate
-- @
--
-- Restrict to x axis only:
--
-- @
-- doDrag Point { x } = translate (Point x 0)
-- @
--
-- Snap to a 10px grid:
--
-- @
-- doDrag Point { x, y } = translate (Point (toGrid x) (toGrid y))
--   where toGrid n = fromInteger $ round n `mod` 10
-- @
--
-- Rotate based on the Euclidean distance the user drags:
--
-- @
-- doDrag p = rotate (Turn $ distance p 0 / 100)
-- @
drags :: forall m d t. Dom t m
      => DragConfig d t
      -- ^ Configuration for how to measure drags.
      -> Element t
      -- ^ The element that can be dragged.
      -> m (Drags t)
      -- ^ Two dynamics that combine to get the net move across /all/
      -- drags:
      --
      --  1. The net movement during the /current/ drag, 'Nothing' if
      --  the element is not being dragged.
      --
      --  2. The net movement from all /finished/ drags, /not/
      --  including the current drag.
drags DragConfig { container, mouseEventFilter } element = do
  -- TODO: Better error handling?
  body <- Document.getBodyUnsafe =<< Dom.askDocument
  (mouseup, mousemove) <- case container of
    Just e  ->
      pure (Dom.domEvent Dom.Mouseup e, Dom.domEvent Dom.Mousemove e)
    Nothing -> do
      up   <- performJs mouseEvent =<< body `on` "mouseup"
      move <- performJs mouseEvent =<< body `on` "mousemove"
      pure (up, move)
  rec let start = gate (not <$> isDragged) (client <$> mousedown)
          end   = gate isDragged           (client <$> mouseup)
          move  = gate isDragged           (client <$> mousemove)

          mousedown = Witherable.filter mouseEventFilter $
            Dom.domEvent Dom.Mousedown element

      isDragged <- Reflex.holdDyn False $ Reflex.leftmost [False <$ end, True <$ start]

      -- without user-select: none, dragging an element can also
      -- highlight text/images/etc—really distracting!
      Reflex.performEvent_ (setUserSelect body "none" <$ start)
      Reflex.performEvent_ (setUserSelect body "auto" <$ end)
      -- TODO: save + restore previous state of body.style.user-select

      -- current drag
      startPosition <- Reflex.holdDyn (Point 0 0) start
      let moves = Reflex.attachWith (flip (-)) (Reflex.current startPosition) move
          resets = Point 0 0 <$ end
      movedBy <- Reflex.holdDyn Nothing $ Just <$> Reflex.leftmost [resets, moves]
      let current = toMaybe <$> isDragged <*> movedBy

      -- completed drags
      let endDelta = Reflex.attachWith (flip (-)) (Reflex.current startPosition) end
      finished <- Reflex.foldDyn (+) (Point 0 0) endDelta

      let total = liftA2 (+) (fromMaybe 0 <$> current) finished

  pure Drags { current, finished, total, start, end }
  where toMaybe dragged delta = if dragged then delta else Nothing
        gate = Reflex.gate . Reflex.current

        setUserSelect e (value :: Text) = do
          style <- Element.getStyle e
          Style.setProperty style ("user-select" :: Text) value (Nothing @Text)
          Style.setProperty style ("-webkit-user-select" :: Text) value (Nothing @Text)

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css demo
