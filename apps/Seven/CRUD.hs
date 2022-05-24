{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Seven.CRUD where

import           Control.Lens
import           Control.Monad     (void)

import qualified Data.ByteString   as BS
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Data.Text.Display

import           Reflex.Dom        hiding (Delete)

import           UI.Element
import qualified UI.PushMap        as PushMap
import           UI.PushMap        (PushMap)
import           UI.Widget

import qualified Witherable        (Filterable (filter))

widget :: forall m t. Dom t m => m ()
widget = elClass "div" "crud" do
  rec let updates = current (zipDynWith act selected entered) <@> action
      names <- foldDynMaybe ($) mempty updates

      prefix   <- filter
      selected <- listbox $ zipDynWith filterPrefix prefix names
      entered  <- elClass "div" "name-input" nameInput
      action   <- crud
  pure ()
  where
    filter = elClass "div" "filter" do
      label "Filter prefix: "
      value <$> inputElement def

    act selected entered action names = case action of
      Create -> pure $ PushMap.push entered names
      Delete -> (PushMap.delete <$> selected) ?? names
      Update -> (PushMap.insert <$> selected) ?? entered ?? names


-- * Controls

-- | The three CRUD operations.
data Crud = Create | Update | Delete
  deriving stock (Show, Eq, Ord)

-- | A control pane with three buttons:
--
--  1. Create
--  2. Update
--  3. Delete
crud :: Dom t m => m (Event t Crud)
crud = elClass "div" "crud-controls" do
  create <- button "Create"
  update <- button "Update"
  delete <- button "Delete"
  pure $ leftmost [Create <$ create, Update <$ update, Delete <$ delete]

-- * Names

-- | A (pretty poor) structured representation of names. This isn't a
-- good approach in general—many people don't have names that fit well
-- into first + surname!—but it'll have to do for a small demo task
-- like this.
data Name = Name { first :: Text, surname :: Text }
  deriving stock (Eq, Ord, Show)

instance Display Name where
  displayBuilder Name { first, surname } =
    displayBuilder first <> " " <> displayBuilder surname

-- | Two textboxes: one for first name, one for surname. Either part
-- can be blank—no validation on the names the user enters.
nameInput :: Dom t m => m (Dynamic t Name)
nameInput = el "div" do
  label "Name: "
  first <- value <$> inputElement def

  label "Surname: "
  surname <- value <$> inputElement def

  pure (zipDynWith Name first surname)

-- | Filter a collection of names, keeping only those where 'last' has
-- the given prefix.
filterPrefix :: Witherable.Filterable f => Text -> f Name -> f Name
filterPrefix prefix = Witherable.filter (Text.isPrefixOf prefix . surname)

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
