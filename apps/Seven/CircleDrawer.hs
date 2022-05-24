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
import           Control.Lens        ((&), (<&>), (??))
import           Control.Monad       (join, void)

import           Data.Bool           (bool)
import qualified Data.ByteString     as BS
import           Data.Default.Class  (def)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe, listToMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Display   (Display, ShowInstance (..))

import           Reflex
import qualified Reflex.Dom          as Dom
import           Reflex.Dom          (dynText)

import qualified Text.Printf         as Text

import           Witherable          (Filterable (..), catMaybes, (<&?>))
import qualified Data.List as List

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  Dom.mainWidgetWithCss css widget

widget :: forall m t. Dom t m => m ()
widget = Dom.elClass "div" "circle-drawer" do
  rec (doUndo, doRedo) <- Dom.elClass "div" "centered controls" do
        doUndo <- button' (constDyn "↶") (enabledIf . hasUndo <$> history)
        doRedo <- button' (constDyn "↷") (enabledIf . hasRedo <$> history)
        pure (doUndo, doRedo)

      (canvas, clicked) <- circlesCanvas circles beingModified

      circles <- foldDyn doAction mempty $
        leftmost [adds, previews, modifies, undos, redos]

      let adds = mainClicks canvas <&> \ MouseEventResult { offset = (x, y) } ->
            AddCircle (fromIntegral x, fromIntegral y)
          modifies =
            catMaybes $ changeRadius <$> current beingModified <@> setRadius
          previews =
            catMaybes $ previewChange <$> current targetCircle <@> updated previewRadius
          undos =
            attachWithMaybe (&) (current history) (toUndo <$ doUndo)
          redos =
            attachWithMaybe (&) (current history) (toRedo <$ doRedo)

      beingModified <- holdDyn Nothing $
        leftmost [Just <$> clicked, Nothing <$ setRadius]

      let targetCircle = zipDynWith getCircle beingModified circles
      (setRadius, previewRadius) <-
        circleDialog beingModified $ fmap snd <$> targetCircle

      history <- foldDyn ($) emptyHistory $ leftmost
        [save <$> modifies, undo <$ doUndo, redo <$ doRedo]

  pure ()
  where previewChange (Just (i, Circle { radius })) newRadius =
          Just $ ChangeRadius i Diff { before = radius, after = newRadius }
        previewChange Nothing _ = Nothing

        changeRadius :: Maybe Int -> (Double, Double) -> Maybe Action
        changeRadius i (old, new) = ChangeRadius <$> i ?? Diff old new

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

-- | An atomic change to a value, recording the value before and after
-- the change.
data Diff a = Diff
  { before :: a
  , after  :: a
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

-- | Undo an action on the given state, if possible.
--
-- Currently, 'AddCircle' and 'ChangeRadius' for an id not in the map
-- cannot be undone. For those actions, this returns the input map
-- unchanged.
undoAction :: Maybe Action -> PushMap Circle -> PushMap Circle
undoAction Nothing circles                        = circles
undoAction (Just AddCircle {}) circles            = circles
undoAction (Just (ChangeRadius i radius)) circles =
  case PushMap.lookup i circles of
    Just c  -> PushMap.insert i c { radius = before radius } circles
    Nothing -> circles

-- ** Undo and Redo

-- | The undo/redo history: actions taken by the user that can be
-- undone and/or redone.
data History = History
  { undos :: [Action]
    -- ^ Past actions that can be undone.
  , redos :: [Action]
    -- ^ Actions that were undone but can be redone.
  }
  deriving stock (Show, Eq)

-- | A history with no actions to undo or redo.
emptyHistory :: History
emptyHistory = History { undos = [], redos = [] }

-- | "Save" an undoable action, allowing the user to undo it.
--
-- This pushes the action to the 'undos' stack and clears the 'redos'
-- stack.
save :: Action -> History -> History
save action history = history { undos = action : undos history, redos = [] }

-- | Update the 'History' to reflect undoing an action.
--
-- Pop the last action off the 'undo' stack (if any) and move it to
-- the top of the 'redo' stack.
--
-- If there is no action to undo, the 'History' is returned unchanged.
undo :: History -> History
undo history@History { undos, redos } = case undos of
  []       -> history
  (a : as) -> history { undos = as, redos = a : redos }

-- | Return the next action that can be undone.
--
-- If 'undos' is empty, return 'Nothing'.
toUndo :: History -> Maybe Action
toUndo History { undos } = flipAction <$> listToMaybe undos
  where flipAction = \case
          ChangeRadius i Diff { before, after } ->
            ChangeRadius i Diff { before = after, after = before }
          a -> a

-- | Update the history to reflect redoing an action.
--
-- Pop the last action of the 'redo' stack (if any) and move it to the
-- top of the 'undo' stack.
--
-- If there is no action to redo, the 'History' is returned unchanged.
redo :: History -> History
redo history@History { undos, redos } = case redos of
  []       -> history
  (a : as) -> history { undos = a : undos, redos = as }

-- | Return the next action that can be redone.
--
-- If 'redos' is empty, return 'Nothing'.
toRedo :: History -> Maybe Action
toRedo = listToMaybe . redos

-- | 'True' if there are actions available to undo, 'False' otherwise.
hasUndo :: History -> Bool
hasUndo = not . List.null . undos

-- | 'True' if there are actions available to redo, 'False' otherwise.
hasRedo :: History -> Bool
hasRedo = not . List.null . redos
