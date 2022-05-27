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

import           Control.Lens       ((<&>), (??))

import qualified Data.ByteString    as BS
import           Data.Default.Class (def)
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as Text
import           Data.Text.Display  (Display, ShowInstance (..))

import           Reflex
import qualified Reflex.Dom         as Dom
import           Reflex.Dom         (dynText)

import qualified Text.Printf        as Text

import           UI.Attributes      (ToAttributes (..), setClass)
import           UI.Dialog          (DialogElement (..), ModalState (..),
                                     dialog)
import           UI.Element         (Dom)
import           UI.Event
import qualified UI.History         as History
import           UI.History         (Undos (..))
import           UI.Point
import qualified UI.PushMap         as PushMap
import           UI.PushMap         (PushMap)
import           UI.SVG
import           UI.Widget

import           Witherable         (Filterable (..), catMaybes, (<&?>))

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css widget

widget :: forall m t. Dom t m => m ()
widget = Dom.elClass "div" "circle-drawer" do
  rec Undos { undoActions, redoActions } <- Dom.elClass "div" "centered controls" $
        History.undoControls modifies

      (canvas, clicked) <- circlesCanvas circles beingModified

      circles <- foldDyn doAction mempty $
        leftmost [adds, previews, modifies, flipChange <$> undoActions, redoActions]

      let adds = mainClicks canvas <&> \ event -> AddCircle (offset event)
          modifies =
            catMaybes $ changeRadius <$> current beingModified <@> setRadius
          previews =
            catMaybes $ previewChange <$> current targetCircle <@> updated previewRadius

      beingModified <- holdDyn Nothing $
        leftmost [Just <$> clicked, Nothing <$ setRadius]

      let targetCircle = zipDynWith getCircle beingModified circles
      (setRadius, previewRadius) <-
        circleDialog beingModified $ fmap snd <$> targetCircle

  pure ()
  where previewChange (Just (i, Circle { radius })) newRadius =
          Just $ ChangeRadius i Diff { before = radius, after = newRadius }
        previewChange Nothing _ = Nothing

        changeRadius :: Maybe Int -> (Double, Double) -> Maybe Action
        changeRadius i (old, new) = ChangeRadius <$> i ?? Diff old new

        flipChange :: Action -> Action
        flipChange (ChangeRadius i Diff { before, after }) =
          ChangeRadius i Diff { before = after, after = before }
        flipChange a = a

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
circlesCanvas circles highlighted = svgAttr' "svg" [("class", "canvas")] do
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
             -- ^ The id of the circle being modified.
             -> Dynamic t (Maybe Circle)
             -- ^ The circle that the dialog controls.
             -> m (Event t (Double, Double), Dynamic t Double)
             -- ^ The 'Event' fires when a modification is saved; the
             -- 'Dynamic' is always up to date with the set radius.
circleDialog beingModified targetCircle = do
  (dialogElement, (old, new)) <- dialog showHide (constDyn []) do
    dynText $ message . center . fromMaybe blank <$> targetCircle

    let oldCircle = tagPromptlyDyn targetCircle showHide
        oldRadius = radius <$> catMaybes oldCircle
    newRadius <- fmap (* maxRadius) <$> range ((/ maxRadius) <$> oldRadius)

    pure (oldRadius, newRadius)

  old' <- holdDyn 0 old
  let both = zipDyn old' new
  pure (current both <@ closed dialogElement, new)
  where showHide = updated beingModified <&> \case
          Just _  -> ShowModal
          Nothing -> Hide

        maxRadius = 500
        message = Text.pack . Text.printf "Adjust diameter of circle at %s" . show

        blank = Circle { center = Point { x = 0, y = 0 }, radius = 0 }

-- * Editing Actions

-- | Editing actions we can take in the UI
data Action = AddCircle !Point
            -- ^ Add a circle with the standard radius at the given
            -- point.
            | ChangeRadius !Int !(Diff Double)
            --              ↑        ↑
            --             id      radius
            -- ^ Change the radius of the circle with the given id.
  deriving stock (Show, Eq)
  deriving Display via (ShowInstance Action)

-- | An atomic change to a value, recording the value before and after
-- the change.
data Diff a = Diff
  { before :: !a
  , after  :: !a
  }
  deriving stock (Show, Eq)

-- | Execute the action on the given state, if possible.
--
-- Some actions like a 'ChangeRadius' for an id that is not in the map
-- cannot be executed, in which case the input map is returned
-- unchanged.
doAction :: Action -> PushMap Circle -> PushMap Circle
doAction (AddCircle center) circles =
  PushMap.push Circle { center, radius = 50 } circles
doAction (ChangeRadius i radius) circles =
  case PushMap.lookup i circles of
    Just c  -> PushMap.insert i c { radius = after radius } circles
    Nothing -> circles
