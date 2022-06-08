module UI.Drop where

import           Control.Monad               (forM, void)

import           Data.Default.Class          (def)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)

import           GHC.Generics                (Generic)

import qualified GHCJS.DOM.Node              as Node

import           Language.Javascript.JSaddle (MonadJSM)

import qualified Reflex
import           Reflex                      (Dynamic, Event)

import qualified UI.Drag                     as Drag
import           UI.Drag                     (DragConfig (..), Drags (..))
import qualified UI.Element                  as Element
import           UI.Element                  (Dom, Html, area, elClass',
                                              elDynAttr', overlap)
import           UI.IsElement                (IsElement, rawElement)
import           UI.Main                     (Runnable (..), withCss)
import           UI.Style                    (duration, property, s,
                                              setProperty, transition,
                                              translate)
import           UI.Widget                   (label, ul)

demo :: forall m t. Dom t m => m ()
demo = void do
  ul (pure [("class", "drop-demo")])
    [ example "Change color on hover and drop" changeColor
    , example "Move element to target" moveElement
    ]
  where example :: Text -> (Html t -> m a) -> m ()
        example description body = do
          label description
          rec (container, _) <- elClass' "div" "draggable drop-example" $
                body container
          pure ()

        changeColor container = do
          rec (target, _)  <- elClass' "div" "drop-target" (pure ())
              (element, _) <- elDynAttr' "div" attributes (pure ())
              wasDropped <- Reflex.holdDyn False $ not . Map.null <$> dropped

              let attributes = do
                    move         <- translate <$> total
                    colorHover   <- colorIf "green" . (not . Map.null) <$> hovering
                    colorDropped <- colorIf "red" <$> wasDropped
                    pure $ move $ colorDropped $ colorHover [("class", "draggable")]

              drags@Drags { total } <-
                Drag.drags def { container = Just container } element
              Drops { hovering, dropped } <-
                drops drags [('a', target)]
          pure ()

        moveElement :: Html t -> m ()
        moveElement container = do
          rec (target, _) <- elClass' "div" "drop-target" (pure ())
              (element, _) <- elDynAttr' "div" attributes (pure ())
              snapping <- Reflex.holdDyn id $
                Reflex.leftmost [id <$ start, snapTo <$ end]
              let attributes = do
                    let base = [("class", "draggable")]
                    move    <- translate . fromMaybe 0 <$> current
                    animate <- snapping
                    pure $ animate $ move base

              drags@Drags { current, start, end } <-
                Drag.drags def { container = Just container } element
              Drops { dropped } <- drops drags [('a', target)]
              Reflex.performEvent_ $ moveTo element target <$ dropped
          pure ()

        snapTo = transition smooth
        smooth = def { property = "transform", duration = s 0.5 }

        colorIf color flag
          | flag      = setProperty "background-color" color
          | otherwise = id

        moveTo (rawElement -> element) (rawElement -> parent) =
          Node.appendChild_ parent element

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
-- dragAndDrop container target = mdo
--   (element, _) <- elDynAttr' "div" attributes (pure ())
--   let attributes = do
--         move <- translate <$> total
--         colorHover   <- colorIf "green" (not . Map.null) <$> hovering
--         colorDropped <- colorIf "red" <$> wasDropped
--         pure $ move $ colorDropped $ colorHover []
--
--   drags@Drags { total } <-
--     Drag.drags def { container = Just container } element
--   Drops { hovering, dropped } <-
--     drops element drags [('a', target)]
--   wasDropped <- Reflex.holdDyn False $ not . Map.null <$> dropped
--   pure ()
-- @
--
-- Move element into drop target:
--
-- @
--
-- @
drops :: forall k e m t. (Ord k, IsElement e, Dom t m)
      => Drags t
      -- ^ drag events + dynamics for the element
      -> Map k e
      -- ^ target elements
      --
      -- The 'Drops' value returned will track the key(s) of the
      -- element(s) being hovered or dropped on.
      -> m (Drops k t)
drops Drags { element, current, end } targets = do
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
  deriving stock (Generic)

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
main = withCss "css/ui-demo.css" (Runnable demo)
