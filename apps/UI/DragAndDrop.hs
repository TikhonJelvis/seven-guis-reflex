{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module UI.DragAndDrop where


import           Control.Monad     (void)
import           Control.Monad.Fix (MonadFix)

import qualified Data.ByteString   as BS
import           Data.Map          (Map)
import           Data.Maybe        (fromMaybe, isJust)
import           Data.Text         (Text)
import qualified Data.Text         as Text

import qualified Reflex
import           Reflex            (Dynamic, Reflex)
import qualified Reflex.Dom        as Dom

import qualified Text.Printf       as Text

import           UI.Attributes     (addClass, setClass, translate,
                                    updateProperty)
import           UI.Element        (Dom, elClass', elDynAttr')
import           UI.Event          (EventResult, MouseButton (..), button,
                                    client)
import           UI.Point          (Point (..))
import           UI.Widget         (label, ul)

import qualified Witherable

demo :: forall m t. Dom t m => m ()
demo = void $ ul (Reflex.constDyn [("class", "drag-demo")])
  [ example "Follow cursor exactly." translate ]
  where example description doDrag = mdo
          label description
          (container, _) <- elClass' "div" "drag-example" mdo
            (element, _) <- elDynAttr' "div" attributes (pure ())
            let attributes = doDrag <$> total <*> Reflex.constDyn []
            Drags { total } <- drags container element
            pure ()
          pure ()

-- | Create an element draggable within the given container.
--
-- We need a containing element because using @mousemove@ events from
-- a container is more robust than using @mousemove@ events from the
-- element being dragged.
--
-- Ideally, the container should be the parent of the draggable
-- element. You can do this using @RecursiveDo@:
--
-- @
-- widget = do
--   rec (container, _) <- elClass' "div" "container" $
--     draggable container "div" (constDyn []) (pure ())
--   pure ()
-- @
draggable :: forall a m t. (Dom t m)
          => Dom.Element EventResult (Dom.DomBuilderSpace m) t
          -- ^ The element will only be draggable while the mouse is
          -- within this container.
          -> Text
          -- ^ The tag for the draggable element
          -> Dynamic t (Map Text Text)
          -- ^ Attributes for the draggable element
          -> m a
          -- ^ Body of the draggable element
          -> m (Dom.Element EventResult (Dom.DomBuilderSpace m) t, a)
draggable container tag attributes body = do
  rec (element, result) <- elDynAttr' tag attributes' body
      let attributes' = setClasses <$> current <*> (translate <$> total <*> attributes)
      Drags { current, total } <- drags container element
  pure (element, result)
  where setClasses (isJust -> dragged) =
          addClass "draggable" . setClass "dragged" dragged

        translate Point {x, y} = updateProperty append "transform" $
          Text.pack $ Text.printf "translate(%fpx, %fpx)" x y
        append a b = b <> " " <> a

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
--   rec let attributes = doDrag <$> total <*> constDyn []
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
-- doDrag Point { x, y } = translate (Point (x `mod` 10) (y `mod` 10))
-- @
--
-- Rotate based on the Euclidean distance the user drags:
--
-- @
-- doDrag p = rotate (Deg $ distance p 0 / 1000)
-- @
drags :: (Reflex t, Reflex.MonadHold t m, MonadFix m)
      => Dom.Element EventResult d t
      -- ^ A container element that restricts where the element
      -- can be dragged.
      -> Dom.Element EventResult d t
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
drags container element = do
  rec let start = gate (not <$> isDragged) (client <$> mousedown)
          end   = gate isDragged           (client <$> mouseup)
          move  = gate isDragged           (client <$> mousemove)

          mousedown = Witherable.filter (\ e -> button e == Main) $
            Dom.domEvent Dom.Mousedown element
          mouseup   = Dom.domEvent Dom.Mouseup container
          mousemove = Dom.domEvent Dom.Mousemove container

      isDragged <- Reflex.holdDyn False $ Reflex.leftmost [False <$ end, True <$ start]

      -- current drag
      startPosition <- Reflex.holdDyn (Point 0 0) start
      let moves = Reflex.attachWith (flip (-)) (Reflex.current startPosition) move
          resets = Point 0 0 <$ end
      movedBy <- Reflex.holdDyn Nothing $ Just <$> Reflex.leftmost [resets, moves]
      let current = toMaybe <$> isDragged <*> movedBy

      -- completed drags
      let endDelta = Reflex.attachWith (flip (-)) (Reflex.current startPosition) end
      finished <- Reflex.foldDyn (+) (Point 0 0) endDelta

      let total = Reflex.zipDynWith (+) (fromMaybe 0 <$> current) finished

  pure Drags { current, finished, total }
  where toMaybe dragged delta = if dragged then delta else Nothing
        gate = Reflex.gate . Reflex.current

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css demo
