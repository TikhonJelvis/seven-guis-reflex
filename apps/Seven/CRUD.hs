module Seven.CRUD where

import           Control.Applicative        (liftA2)
import           Control.Lens               ((??))
import           Control.Monad              (void)

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Display          (Display (displayBuilder))

import qualified Reflex
import           Reflex                     (Dynamic, Event, (<@>))

import           UI.Attributes              (class_, id_)
import           UI.Attributes.AttributeSet ((=:))
import           UI.Element                 (Dom)
import qualified UI.Html                    as Html
import qualified UI.Html.Input              as Input
import           UI.Main                    (Runnable (..), withCss)
import qualified UI.PushMap                 as PushMap
import           UI.Widget                  (listbox)

import qualified Witherable                 (Filterable (filter))

widget :: forall m t. Dom t m => m ()
widget = void $ Html.div_ [ class_ =: ["crud"] ] do
  rec let updates = Reflex.current (liftA2 act selected entered) <@> action
      names <- Reflex.foldDynMaybe ($) mempty updates

      prefix   <- prefixEntry
      selected <- listbox $ liftA2 filterPrefix prefix names
      entered  <- snd <$> Html.div_ [ class_ =: ["name-input"] ] nameInput
      action   <- crud
  pure ()
  where
    prefixEntry = snd <$> Html.div_ [ class_ =: ["filter"] ] do
      Html.labelFor "prefix" "Filter prefix: "
      snd <$> Input.text [ id_ =: "prefix" ] Reflex.never

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
crud = snd <$> Html.div_ [ class_ =: ["crud-controls"] ] do
  create <- snd <$> Html.button' "Create" []
  update <- snd <$> Html.button' "Update" []
  delete <- snd <$> Html.button' "Delete" []
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
nameInput = snd <$> Html.div_ [] do
  Html.labelFor "first-name" "Name: "
  first <- snd <$> Input.text [ id_ =: "first-name" ] Reflex.never

  Html.labelFor "surname" "Surname: "
  surname <- snd <$> Input.text [ id_ =: "surname" ] Reflex.never

  pure (liftA2 Name first surname)

-- | Filter a collection of names, keeping only those where 'last' has
-- the given prefix.
filterPrefix :: Witherable.Filterable f => Text -> f Name -> f Name
filterPrefix prefix = Witherable.filter (Text.isPrefixOf prefix . surname)

main :: IO ()
main = withCss "css/tasks.css" (Runnable widget)
