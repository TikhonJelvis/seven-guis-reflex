{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Seven.CircleDrawer where

import           Seven.Attributes    (ToAttributes (..))
import           Seven.Dialog        (DialogElement (..), ModalState (..),
                                      dialog)
import           Seven.Element
import           Seven.Event
import qualified Seven.PushMap       as PushMap
import           Seven.PushMap       (PushMap)
import           Seven.SVG
import           Seven.Widget

import           Control.Applicative ((<|>))
import           Control.Lens        ((<&>), (??))
import           Control.Monad       (join, void)

import           Data.Bool           (bool)
import qualified Data.ByteString     as BS
import           Data.Default.Class  (def)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Display   (Display, ShowInstance (..))

import           Reflex
import qualified Reflex.Dom          as Dom
import           Reflex.Dom          (dynText)

import qualified Text.Printf         as Text

import           Witherable          (Filterable (..), catMaybes, (<&?>))

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css widget

widget :: forall m t. Dom t m => m ()
widget = Dom.elClass "div" "circle-drawer" do
  action <- Dom.elClass "div" "centered controls" do
    undos <- Dom.button "↶"
    redos <- Dom.button "↷"
    pure $ leftmost [undos, redos]

  rec (canvas, clicked) <- circlesCanvas circles beingModified
      circles <- foldDyn applyAction mempty $
        leftmost [Just <$> adds, previews, modifies]

      let adds = mainClicks canvas <&> \ MouseEventResult { offset = (x, y) } ->
            AddCircle (fromIntegral x, fromIntegral y)
          modifies =
            changeRadius <$> current modifiedCircle <@> setRadius
          previews =
            changeRadius <$> current modifiedCircle <@> updated previewRadius

      beingModified <- holdDyn Nothing $
        leftmost [Just <$> clicked, Nothing <$ setRadius]

      let modifiedCircle = zipDynWith getCircle beingModified circles
      (setRadius, previewRadius) <-
        circleDialog beingModified $ fmap snd <$> modifiedCircle
  pure ()
  where changeRadius (Just (i, Circle { radius })) newRadius =
          Just $ ChangeRadius i Diff { before = radius, after = newRadius }
        changeRadius Nothing _ = Nothing

        getCircle (Just i) circles = (i,) <$> PushMap.lookup i circles
        getCircle Nothing _        = Nothing

        mainClicks = Witherable.filter isMain . Dom.domEvent Dom.Click
        isMain e = button e == Main

-- | The SVG element where the circles are rendered.
--
-- Returns:
--
--  * the containing SVG element
--  * an 'Event' that fires when a circle is middle clicked
circlesCanvas :: forall m t. Dom t m
              => Dynamic t (PushMap Circle)
              -- ^ The full set of circles to render.
              -> Dynamic t (Maybe Int)
              -- ^ Which circle, if any, should be highlighted.
              -> m ( Dom.Element EventResult Dom.GhcjsDomSpace t
                   , Event t Int )
circlesCanvas circles highlighted = svg' "svg" do
  rawClicks <- selectView highlighted circles (withId svgCircle)
  let clicks = rawClicks <&?> \ (MouseEventResult { button }, i) ->
        [i | button == Auxiliary]
  pure clicks

-- | Render an SVG circle.
--
-- Returns an 'Event' that triggers each time the circle is clicked.
svgCircle :: forall m t. Dom t m
          => Dynamic t Circle
          -> Dynamic t Bool
          -> m (Event t MouseEventResult)
svgCircle c isSelected = do
  element <- circle c $ class_ <*> constDyn defaults
  pure $ Dom.domEvent Dom.Click element
  where class_ = setClass "selected" <$> isSelected
        defaults = toAttributes def { width = 2 }

-- | The dialog that lets us control the radius of a cricle.
--
-- The dialog will be shown as a modal each time the input 'Dynamic'
-- updates to a 'Just', and it will be closed each time the input
-- 'Dynamic' updates to a 'Nothing'.
--
-- The function returns two values:
--
--  * A 'Dynamic' with the radius set for the circle
--
--  * An 'Event' that fires with the final radius when the dialog is
--  * closed.
circleDialog :: forall m t. Dom t m
             => Dynamic t (Maybe Int)
             -> Dynamic t (Maybe Circle)
             -- ^ The circle that the dialog controls.
             -> m (Event t Double, Dynamic t Double)
circleDialog beingModified targetCircle = do
  (dialogElement, newRadius) <- dialog showHide (constDyn []) do
    dynText $ message . center . fromMaybe blank <$> targetCircle
    rec let baseRadius = current targetCircle <@ updated beingModified
            setRadius = (/ maxRadius) . radius <$> catMaybes baseRadius
        new <- fmap (* maxRadius) <$> range setRadius
    pure new

  pure (current newRadius <@ closed dialogElement, newRadius)
  where showHide = updated beingModified <&> \case
          Just _  -> ShowModal
          Nothing -> Hide

        maxRadius = 500
        message = Text.pack . Text.printf "Adjust diameter of circle at %s" . show

        blank = Circle { center = (0, 0), radius = 0 }

-- * Editing Actions

-- | Editing actions we can take in the UI
data Action = AddCircle (Double, Double)
            -- ^ Add a circle with the standard radius at the given
            -- point.
            | ChangeRadius Int (Diff Double)
            --              ↑        ↑
            --             id      radius
            -- ^ Change the radius of the circle with the given id.
  deriving stock (Show, Eq)
  deriving Display via (ShowInstance Action)

-- | Execute the action, if possible, on the given state.
--
-- Some actions like a 'ChangeRadius' for an id that is not in the map
-- cannot be executed, in which case the input map is returned
-- unchanged.
applyAction :: Maybe Action -> PushMap Circle -> PushMap Circle
applyAction Nothing circles = circles
applyAction (Just (AddCircle center)) circles =
  PushMap.push Circle { center, radius = 50 } circles
applyAction (Just (ChangeRadius i radius)) circles =
  case PushMap.lookup i circles of
    Just c  -> PushMap.insert i c { radius = after radius } circles
    Nothing -> circles

-- | An atomic change to a value, recording the value before and after
-- the change.
data Diff a = Diff
  { before :: a
  , after  :: a
  }
  deriving stock (Show, Eq)
