module UI.Html.Select
  ( -- * Select Elements
    HtmlSelect
  , select

    -- ** Options
  , Entry (..)
  , disabled
  , selected
  , option
  , option'
  , placeholder
  , group
  )
where

import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import           Data.Text.Display       (Display, display)

import qualified Reflex

import           UI.Attributes           (AsAttributeValue)
import qualified UI.Element              as Element
import qualified UI.Html                 as Html
import qualified UI.Html.Input           as Input
import           UI.Html.Select.Internal
import           UI.Main                 (Runnable (Runnable), withCss)


-- | Make an option or group disabled by default.
--
-- __Example__
--
-- A 'select' with a mix of enabled and disabled options and groups:
--
-- @
-- select [] Nothing never
--   [ placeholder "--please select an ingredient--"
--   , group "Fruit"
--     [ option "Apple"
--     , option "Pear"
--     , disabled (option "Banana")
--     ]
--   , disabled $ group "Vegetables"
--     [ disabled (option "Cucumber")
--     , option "Squash" -- disabled since group is disabled
--     ]
--   ]
-- @
disabled :: forall k. Entry k -> Entry k
disabled = \case
  Option _ isSelected label key -> Option Input.Disabled isSelected label key
  Group _ label options         -> Group Input.Disabled label options

-- | Make an option selected by default.
--
-- Does not do anything if applied to a group.
--
-- Note: the initially selected value is currently /not/ reflected in
-- the 'Dynamic' returned from 'select'. Instead, the initial value
-- passed to 'select' is used until some event changes the value. This
-- is a limitation of reflex-dom's 'Dom.selectElement' function so it
-- will hopefully change in the future.
--
-- __Example__
--
-- A 'select' with the @"Apple"@ option selected by default:
--
-- @
-- select [] (Just "Apple") never
--   [ selected (option "Apple")
--   , option "Pear"
--   ]
-- @
selected :: forall k. Entry k -> Entry k
selected = \case
  Option enabled _ label key -> Option enabled True label key
  g@Group{}                  -> g

-- | An option whose user-facing label is the 'display' of its key.
--
-- __Examples__
--
-- An option labeled @true@:
--
-- @
-- option "true"
-- @
--
-- An option labeled @false@ and disabled:
--
-- @
-- disabled (option "false")
-- @
option :: forall k. (AsAttributeValue k, Display k) => k -> Entry k
option key = Option Input.Enabled False (Just key) (display key)

-- | An option with the given key and user-facing label.
--
-- __Example__
--
-- An select with two options labeled "yes" or "no", mapping to 'True'
-- and 'False' respectively:
--
-- @
-- select [] (Just "yes") never
--   [ selected (option' True "yes")
--   , option' False "no"
--   ]
-- @
option' :: forall k. AsAttributeValue k => k -> Text -> Entry k
option' key = Option Input.Enabled False (Just key)

-- | An option with @value=""@, useful for placeholders that do not
-- correspond to a valid selection.
--
-- If the @select@ element has @required@ set, a 'placeholder' option
-- will not count as a selection.
--
-- __Examples__
--
-- A placeholder:
--
-- @
-- placeholder "--please select an option--"
-- @
placeholder :: forall k. Text -> Entry k
placeholder = Option Input.Enabled False Nothing

-- | A group of options (@optgroup@ element), enabled by default.
--
-- __Examples__
--
-- A group of options:
--
-- @
-- group "Fruit"
--   [ option "Apple"
--   , option "Pear"
--   ]
-- @
group :: forall k. Text -> [Entry k] -> Entry k
group = Group Input.Enabled


_demo :: IO ()
_demo = do
  withCss "css/ui.css" $ Runnable do
    Html.div_ [] do
      Html.label [] do
        Element.text "No default:"
        (_, selectedValue) <- select [] Nothing Reflex.never
          [ placeholder "--please select an ingredient--"
          , group "Fruit"
            [ option "Apple"
            , option "Pear"
            , disabled (option "Banana")
            ]
          , disabled $ group "Vegetables"
            [ disabled (option "Cucumber")
            , option "Squash"
            ]
          ]
        Element.dynText (fromMaybe "<none>" <$> selectedValue)

    Html.div_ [] do
      Html.label [] do
        Element.text "With default selected:"
        (_, selectedValue) <- select [] (Just "Pear") Reflex.never
          [ group "Fruit"
            [ option "Apple"
            , option "Pear"
            , disabled (option "Banana")
            ]
          , disabled $ group "Vegetables"
            [ disabled (option "Cucumber")
            , option "Squash"
            ]
          ]
        Element.dynText (fromMaybe "<none>" <$> selectedValue)
