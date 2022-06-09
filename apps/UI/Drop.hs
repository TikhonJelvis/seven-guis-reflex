{-# LANGUAGE MonadComprehensions #-}
module UI.Drop where

import           Control.Monad               (void)

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

import qualified Witherable

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
                    colorHover   <- colorIf "green" . isHovering <$> hovering
                    colorDropped <- colorIf "red" <$> wasDropped
                    pure $ move $ colorDropped $ colorHover [("class", "draggable")]
                  isHovering = maybe False (> 0) . Map.lookup 0

              drags@Drags { total } <-
                Drag.drags def { container = Just container } element
              Drops { hovering, dropped } <- drops target $ pure [(0, drags)]
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
              Drops { dropped } <- drops target $ pure [(0, drags)]
              Reflex.performEvent_ $ moveTo element target <$ dropped
          pure ()

        snapTo = transition smooth
        smooth = def { property = "transform", duration = s 0.5 }

        colorIf color flag
          | flag      = setProperty "background-color" color
          | otherwise = id

        moveTo (rawElement -> element) (rawElement -> parent) =
          Node.appendChild_ parent element

-- | Keeps track of how draggable elements are dropped onto a
-- target. The set of droppable elements can change over time.
--
-- The set of elements is tracked as a map of 'Drags' values, as
-- returned by 'Drag.drags'. Each droppable element is associated with
-- a key to track /which/ element was dropped.
--
-- An element is considered /over/ the target when its bounding box
-- overlaps with the target to any extent.
--
-- __Examples__
drops :: forall k e m t. (Ord k, IsElement e, Dom t m)
      => e
      -- ^ the target element
      -> Dynamic t (Map k (Drags t))
      -- ^ a dynamic set of droppable elements
      -> m (Drops k t)
drops target droppables = do
  let elements = Reflex.switchDyn $ mergeWith droppedElement
  dropped <- Reflex.performEvent $ getOverlaps (> 0) <$> elements

  let active = Reflex.switchDyn $ mergeWith draggedElement
  hoverEvents <- Reflex.performEvent $ getOverlaps (const True) <$> active
  hovering    <- Reflex.holdDyn mempty hoverEvents

  pure Drops { dropped, hovering }
  where mergeWith f = Reflex.mergeMap . fmap f <$> droppables

        droppedElement :: Drags t -> Event t (Html t)
        droppedElement Drags { element, end } =
          element <$ end

        draggedElement :: Drags t -> Event t (Html t)
        draggedElement Drags { element, current } =
          element <$ Reflex.updated current

        getOverlaps :: MonadJSM m'
                    => (Overlap -> Bool)
                    -> Map k (Html t)
                    -> m' (Map k Overlap)
        getOverlaps cond = Witherable.witherM \ e -> do
          proportion <- overlapProportion e target
          pure [proportion | cond proportion]

-- | Tracking how a set of droppable elements interacts with a drop
-- target.
data Drops k t = Drops
  { dropped  :: Event t (Map k Overlap)
    -- ^ Fires when a droppable element is dropped and overlaps the
    -- target.
    --
    -- Returns a map of keys in case multiple elements are dropped
    -- simultaneously. Only the elements that were dropped /over the
    -- target/ will be in the map.
    --
    -- This event will /only/ fire when a droppable element's 'end'
    -- event fires. If the element's 'end' event fires but it does not
    -- overlap the target, the corresponding 'dropped' event will
    -- /not/ fire, so the overlap reported will always be > 0.

  , hovering :: Dynamic t (Map k Overlap)
    -- ^ A map containing each /actively dragged element/ and how much
    -- it overlaps the target. If an element is being dragged but is
    -- not over the target, its overlap will be 0.
    --
    -- If an element is /not/ being dragged (ie 'current' is
    -- 'Nothing'), it will not be in this map.
  }
  deriving stock (Generic)

-- * Drop Calculations

-- | The proportion of the /droppable/ element's __bounding box__ that
-- overlaps over the /target/ element's bounding box. Will always be
-- in the range (0, 1].
--
-- See 'bounds' and MDN
-- [getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
type Overlap = Double

-- | The proportion of one element's __bounding box__ overlapping
-- another element's bounding box. Will always be in the range [0, 1].
--
-- For 'Drops', this will be the proportion of the /droppable/ element
-- that overlaps the /target/ element.
--
-- See 'bounds' and MDN
-- [getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
overlapProportion :: forall e e' m. (IsElement e, IsElement e', MonadJSM m)
        => e
        -- ^ Droppable element
        --
        -- The returned 'Overlap' is a fraction of /this/ element's
        -- bounding box
        -> e'
        -- ^ Target element
        -> m Double
overlapProportion element target = do
  droppedBounds <- Element.bounds element
  targetBounds  <- Element.bounds target
  pure case overlap droppedBounds targetBounds of
    Just overlapped -> area overlapped / area droppedBounds
    Nothing         -> 0

main :: IO ()
main = withCss "css/ui-demo.css" (Runnable demo)
