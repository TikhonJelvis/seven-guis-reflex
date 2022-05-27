{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI.DragAndDrop where


import qualified Data.ByteString as BS
import           Data.Map        (Map)
import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Reflex          (Dynamic, attachWith, constDyn, current, gate,
                                  holdDyn, leftmost, performEvent, zipDynWith)
import qualified Reflex.Dom      as Dom

import qualified Text.Printf     as Text

import           UI.Attributes   (addClass, setClass, setProperty)
import           UI.Element      (Dom, elClass', elDynAttr', offsetPosition)
import           UI.Event        (EventResult, MouseButton (..), button, client)
import           UI.Point        (Point (..))

import qualified Witherable

widget :: forall m t. Dom t m => m ()
widget = do
  rec (container, _) <- elClass' "div" "field" do
        card container $ pure ()
  pure ()
  where card container = draggable container "div" (constDyn [("class", "card")])

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
      let attributes' = setClasses <$> held <*> (setPosition <$> position <*> attributes)

      let mousedown = Witherable.filter (\ e -> button e == Main) $
            Dom.domEvent Dom.Mousedown element
          mouseup   = Dom.domEvent Dom.Mouseup container
          mousemove = Dom.domEvent Dom.Mousemove container

      held <- holdDyn False $
        leftmost [False <$ mouseup, True <$ mousedown]

      -- note: this fires /after/ mousedown
      offsets <- performEvent (offsetPosition element <$ mousedown)

      startPosition <- holdDyn (Point 0 0) offsets
      dragStart     <- holdDyn (Point 0 0) $ client <$> mousedown

      let moves = attachWith (-) (current dragStart) $ client <$> mousemove
          resets = Point 0 0 <$ offsets
      delta <- holdDyn (Point 0 0) $
        leftmost [resets, gate (current held) moves]
      let position = zipDynWith (-) startPosition delta

  pure (element, result)
  where setClasses dragged = addClass "draggable" . setClass "dragged" dragged

        setPosition Point { x, y } =
          setProperty "left" (px x) . setProperty "top" (px y)

        px = Text.pack . Text.printf "%fpx"

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css widget
