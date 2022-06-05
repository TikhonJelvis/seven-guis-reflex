module Seven.CRUD where

import           Control.Applicative (liftA2)
import           Control.Lens        ((??))

import           Data.Default.Class  (def)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Display   (Display (displayBuilder))

import qualified Reflex
import           Reflex              (Dynamic, Event, (<@>))
import qualified Reflex.Dom          as Dom

import           UI.Element          (Dom)
import           UI.Main             (Runnable (..), withCss)
import qualified UI.PushMap          as PushMap
import           UI.Widget           (label, listbox)

import qualified Witherable          (Filterable (filter))

widget :: forall m t. Dom t m => m ()
widget = Dom.elClass "div" "crud" do
  rec let updates = Reflex.current (liftA2 act selected entered) <@> action
      names <- Reflex.foldDynMaybe ($) mempty updates

      prefix   <- prefixEntry
      selected <- listbox $ liftA2 filterPrefix prefix names
      entered  <- Dom.elClass "div" "name-input" nameInput
      action   <- crud
  pure ()
  where
    prefixEntry = Dom.elClass "div" "filter" do
      label "Filter prefix: "
      Dom.value <$> Dom.inputElement def

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
crud = Dom.elClass "div" "crud-controls" do
  create <- Dom.button "Create"
  update <- Dom.button "Update"
  delete <- Dom.button "Delete"
  pure $ Reflex.leftmost [Create <$ create, Update <$ update, Delete <$ delete]

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
nameInput = Dom.el "div" do
  label "Name: "
  first <- Dom.value <$> Dom.inputElement def

  label "Surname: "
  surname <- Dom.value <$> Dom.inputElement def

  pure (liftA2 Name first surname)

-- | Filter a collection of names, keeping only those where 'last' has
-- the given prefix.
filterPrefix :: Witherable.Filterable f => Text -> f Name -> f Name
filterPrefix prefix = Witherable.filter (Text.isPrefixOf prefix . surname)

main :: IO ()
main = withCss "css/tasks.css" (Runnable widget)
