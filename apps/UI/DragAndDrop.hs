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

import           UI.Attributes     (addClass, setClass, updateProperty)
import           UI.Element        (Dom, elClass', elDynAttr')
import           UI.Event          (EventResult, MouseButton (..), button,
                                    client)
import           UI.Point          (Point (..))

import qualified Witherable

widget :: forall m t. Dom t m => m ()
widget = do
  rec (container, _) <- elClass' "div" "field" do
        card container $ pure ()
  pure ()
  where card container = draggable container "div" (Reflex.constDyn [("class", "card")])

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
      let attributes' = setClasses <$> drags <*> (translate <$> total <*> attributes)
      (drags, finished) <- dragging container element
      let total = Reflex.zipDynWith (+) (fromMaybe 0 <$> drags) finished
  pure (element, result)
  where setClasses (isJust -> dragged) =
          addClass "draggable" . setClass "dragged" dragged

        translate Point {x, y} = updateProperty append "transform" $
          Text.pack $ Text.printf "translate(%fpx, %fpx)" x y
        append a b = b <> " " <> a

-- | Keeps track of how a user drags an element.
--
-- A "drag" starts when the user holds the main mouse button over the
-- element and ends when the user releases the mouse button.
--
-- During a drag, this will 'Just' be a 'Point' that reflects the
-- total movement during /that/ drag.
--
-- When a user is not dragging the element, this will be 'Nothing'.
dragging :: (Reflex t, Reflex.MonadHold t m, MonadFix m)
         => Dom.Element EventResult d t
         -- ^ A container element that restricts where the element
         -- can be dragged.
         -> Dom.Element EventResult d t
         -- ^ The element that can be dragged.
         -> m (Dynamic t (Maybe Point), Dynamic t Point)
         -- ^ Two dynamics: one for the mouse movement during the
         -- /current drag/ (and 'Nothing' otherwise) and one for the
         -- /total finished/ drags.
dragging container element = do
  rec let start = gate (not <$> isDragged) (client <$> mousedown)
          end   = gate isDragged           (client <$> mouseup)
          move  = gate isDragged           (client <$> mousemove)

      isDragged <- Reflex.holdDyn False $ Reflex.leftmost [False <$ end, True <$ start]

      -- current drag
      startPosition <- Reflex.holdDyn (Point 0 0) start
      let moves = Reflex.attachWith (flip (-)) (Reflex.current startPosition) move
          resets = Point 0 0 <$ end
      movedBy <- Reflex.holdDyn Nothing $ Just <$> Reflex.leftmost [resets, moves]

      -- completed drags
      let endDelta = Reflex.attachWith (flip (-)) (Reflex.current startPosition) end
      totalMoved <- Reflex.foldDyn (+) (Point 0 0) endDelta

  pure (toMaybe <$> isDragged <*> movedBy, totalMoved)
  where mousedown = Witherable.filter (\ e -> button e == Main) $
          Dom.domEvent Dom.Mousedown element
        mouseup = Dom.domEvent Dom.Mouseup container
        mousemove = Dom.domEvent Dom.Mousemove container

        toMaybe dragged delta = if dragged then delta else Nothing

        gate = Reflex.gate . Reflex.current

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css widget
