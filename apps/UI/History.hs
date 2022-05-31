{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities for maintaining undo/redo history.
module UI.History where

import           Control.Lens  ((&))

import           Data.Hashable (Hashable)
import qualified Data.List     as List
import           Data.Maybe    (listToMaybe)

import           GHC.Generics  (Generic)

import qualified Reflex
import           Reflex        (Dynamic, Event)

import           UI.Element
import           UI.Widget

-- * History

-- | The undo/redo history: actions taken by the user that can be
-- undone and/or redone.
data History a = History
  { undos :: [a]
    -- ^ Past actions that can be undone.
  , redos :: [a]
    -- ^ Actions that were undone but can be redone.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A history with no actions to undo or redo.
empty :: History a
empty = History { undos = [], redos = [] }

-- | "Save" an undoable action, allowing the user to undo it.
--
-- This pushes the action to the 'undos' stack and clears the 'redos'
-- stack.
save :: a -> History a -> History a
save action history = history { undos = action : undos history, redos = [] }

-- | Update the 'History' to reflect undoing an action.
--
-- Pop the last action off the 'undo' stack (if any) and move it to
-- the top of the 'redo' stack.
--
-- If there is no action to undo, the 'History' is returned unchanged.
undo :: History a -> History a
undo history@History { undos, redos } = case undos of
  []       -> history
  (a : as) -> history { undos = as, redos = a : redos }

-- | Return the next action that can be undone.
--
-- If 'undos' is empty, return 'Nothing'.
toUndo :: History a -> Maybe a
toUndo History { undos } = listToMaybe undos

-- | Update the history to reflect redoing an action.
--
-- Pop the last action of the 'redo' stack (if any) and move it to the
-- top of the 'undo' stack.
--
-- If there is no action to redo, the 'History' is returned unchanged.
redo :: History a -> History a
redo history@History { undos, redos } = case redos of
  []       -> history
  (a : as) -> history { undos = a : undos, redos = as }

-- | Return the next action that can be redone.
--
-- If 'redos' is empty, return 'Nothing'.
toRedo :: History a -> Maybe a
toRedo = listToMaybe . redos

-- | 'True' if there are actions available to undo, 'False' otherwise.
hasUndo :: History a -> Bool
hasUndo = not . List.null . undos

-- | 'True' if there are actions available to redo, 'False' otherwise.
hasRedo :: History a -> Bool
hasRedo = not . List.null . redos

-- * Controls

-- | A button with the label @"↶"@ that fires an event with the action
-- to undo when pressed.
undoButton :: forall a m t. Dom t m
           => Dynamic t (History a)
           -- ^ Current 'History'
           -> m (Event t a)
           -- ^ Event with the action to undo
undoButton history = do
  pressed <- button' (pure "↶") (enabledIf . hasUndo <$> history)
  pure $ Reflex.attachWithMaybe (&) (Reflex.current history) (toUndo <$ pressed)

-- | A button with the label @"↷"@ that fires an event with the action
-- to redo when pressed.
redoButton :: forall a m t. Dom t m
           => Dynamic t (History a)
           -- ^ Current 'History'
           -> m (Event t a)
           -- ^ Event with the action to undo
redoButton history = do
  pressed <- button' (pure "↷") (enabledIf . hasRedo <$> history)
  pure $ Reflex.attachWithMaybe (&) (Reflex.current history) (toRedo <$ pressed)

-- | A control pane with an undo button and a redo button that manage
-- history based on the given input events. Each input event that
-- fires becomes an action that can be undone and redone.
undoControls :: forall a m t. Dom t m
             => Event t a
             -- ^ Undoable actions to save in 'History'
             -> m (Undos t a)
undoControls actions = do
  rec undoActions <- undoButton history
      redoActions <- redoButton history
      history <- Reflex.foldDyn ($) empty $ Reflex.leftmost
        [save <$> actions, undo <$ undoActions, redo <$ redoActions]
  pure Undos { undoActions, redoActions, history }

-- | Everything needed for managing undos: the current 'History' as
-- well as events for undo and redo actions.
data Undos t a = Undos
  { history     :: Dynamic t (History a)
  -- ^ The current history of undoable and redoable actions.
  , undoActions :: Event t a
  -- ^ An event that fires with the action to undo.
  , redoActions :: Event t a
  -- ^ An event that fires with the action to redo.
  }
  deriving stock (Generic)
