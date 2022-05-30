{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module UI.Drop where

import           Control.Monad               (forM, void)

import qualified Data.ByteString             as BS
import           Data.Default.Class          (def)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map

import           Language.Javascript.JSaddle (MonadJSM)

import qualified Reflex
import           Reflex                      (Dynamic, Event)
import qualified Reflex.Dom                  as Dom

import qualified UI.Drag                     as Drag
import           UI.Drag                     (DragConfig (..), Drags (..))
import qualified UI.Element                  as Element
import           UI.Element                  (Dom, area, elClass', elDynAttr',
                                              overlap)
import           UI.IsElement                (IsElement)
import           UI.Style                    (setProperty, translate)

demo :: forall m t. Dom t m => m ()
demo = void do
  rec (container, _) <- elClass' "div" "draggable drop-example" do
        rec (target, _)  <- elClass' "div" "drop-target" (pure ())
            (element, _) <- elDynAttr' "div" attributes (pure ())
            wasDropped <- Reflex.holdDyn False $ not . Map.null <$> dropped

            let attributes = do
                  move <- translate <$> total
                  colorHover <- colorIf "green" . (not . Map.null) <$> hovering
                  colorDropped <- colorIf "red" <$> wasDropped
                  pure $ move $ colorDropped $ colorHover [("class", "draggable")]

            drags@Drags { total } <-
              Drag.drags def { container = Just container } element
            Drops { hovering, dropped } <-
              drops element drags [('a', target)]
        pure ()
  pure ()
  where colorIf color flag
          | flag      = setProperty "background-color" color
          | otherwise = id

-- | Keeps track of how a draggable element is dropped on a set of
-- target elements.
--
-- This is designed to be combined with 'UI.Drag.drags', providing a
-- flexible foundation for drag-and-drop logic.
--
-- __Examples__
--
-- Set up a draggable element with a drop target; change the element's
-- color when it's hovering and again after it's been dropped.
--
-- @
-- dragAndDrop = do
--   rec let attributes = do
--     move <- translate <$> total
--     color <- highlight <$> dragStatus
--     pure (move $ color [])
--
-- @
drops :: forall k e e' m t. (Ord k, IsElement e, IsElement e', Dom t m)
      => e
      -- ^ draggable element
      -> Drags t
      -- ^ drag events + dynamics for the element
      -> Map k e'
      -- ^ target elements
      --
      -- The 'Drops' value returned will track the key(s) of the
      -- element(s) being hovered or dropped on.
      -> m (Drops k t)
drops element Drags { current, end } targets = do
  dropped  <- Reflex.performEvent $ getOverlaps <$ end
  dragged  <- Reflex.performEvent $ getOverlaps <$ Reflex.updated current
  hovering <- Reflex.holdDyn [] dragged
  pure Drops { dropped, hovering }
  where getOverlaps = do
          overlaps <- forM (Map.toList targets) \ (k, target) ->
            (k,) <$> overlapProportion element target
          pure $ Map.fromList
            [(k, overlapped) | (k, overlapped) <- overlaps, overlapped /= 0]

-- | Information about how an element is dropped on a set of targets.
data Drops k t = Drops
  { dropped  :: Event t (Map k Double)
    -- ^ An event that will fire each time a drop is detected. Each
    -- target the element overlaps will have its overlap area in the
    -- map.
    --
    -- This event will /only/ fire when the input event to 'drops'
    -- fires.

  , hovering :: Dynamic t (Map k Double)
    -- ^ A dynamic that tracks which targets are being hovered the
    -- dragged element, and how much the element overlaps the target.
  }

-- * Drop Calculations

-- | Calculate how much of an element overlaps the target element.
overlapProportion :: forall e e' m. (IsElement e, IsElement e', MonadJSM m)
        => e
        -- ^ Dropped element
        -> e'
        -- ^ Target element
        -> m Double
overlapProportion element target = do
  droppedBounds <- Element.bounds element
  targetBounds  <- Element.bounds target
  pure $ maybe 0 area (overlap droppedBounds targetBounds) / area droppedBounds


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css demo
