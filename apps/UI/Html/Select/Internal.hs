-- | Internal implementation of the @select@ API.
--
-- Some of the functionality exported in this can create
-- invalid/incoherent elements if used incorrectly, which should be
-- prevented by the API exposed through the 'UI.Html.Select' module.
module UI.Html.Select.Internal where

import           Control.Monad                     (forM_)
import           Control.Monad.State               (evalState, get, put)

import           Data.Hashable                     (Hashable)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import           Data.Text.Display                 (Display, display)

import           GHC.Generics                      (Generic)

import qualified GHCJS.DOM.Types                   as GHCJS

import qualified Reflex
import           Reflex                            (Dynamic, Event, Reflex)
import qualified Reflex.Dom                        as Dom

import           UI.Attributes                     (AsAttributeValue, boolean,
                                                    fromAttributeValue, native,
                                                    toAttributeValue)
import           UI.Attributes.AttributeSet.Reflex (AttributeSet, toDom, (=:))
import qualified UI.Element                        as Element
import           UI.Element                        (Dom, createSelectElement)
import           UI.Element.IsElement              (IsElement (..), IsHtml (..))
import qualified UI.Event                          as Event
import qualified UI.Html                           as Html
import           UI.Html                           (Html)
import qualified UI.Html.Input                     as Input
import           UI.Html.Input                     (Enabled)
import           UI.Main                           (Runnable (Runnable),
                                                    withCss)

-- * Select Elements

-- | An HTML DOM @select@ element (@HTMLSelectElement@ in JavaScript).
newtype HtmlSelect t = HtmlSelect
  (Dom.SelectElement Event.EventResult Dom.GhcjsDomSpace t)
  deriving stock (Generic)

instance Dom.HasValue (HtmlSelect t) where
  type Value (HtmlSelect t) = Dynamic t Text
  value (HtmlSelect e) = Dom._selectElement_value e

instance IsElement (HtmlSelect t) where
  rawElement (HtmlSelect e) = Dom._element_raw $ Dom._selectElement_element e

instance IsHtml (HtmlSelect t) where
  rawHtml = GHCJS.uncheckedCastTo GHCJS.HTMLElement . rawElement

instance Reflex t => Dom.HasDomEvent t (HtmlSelect t) en where
  type DomEventType (HtmlSelect t) en = Event.EventResultType en

  domEvent eventName (HtmlSelect e) =
    Dom.domEvent eventName $ Dom._selectElement_element e

-- | A drop-down select menu with a set of options.
select :: forall k m t. (Eq k, AsAttributeValue k, Dom t m)
       => AttributeSet t "select" "HTML"
       -- ^ Attributes
       -> Maybe k
       -- ^ Initial selected attribute (optional).
       --
       -- When set, the /first/ attribute with the given value will be
       -- marked as selected. If no attribute has a corresponding key,
       -- none will be selected.
       -> Event t (Maybe k)
       -- ^ Explicitly override the selected value.
       -> [Entry k]
       -- ^ Options to display to the user.
       -> m (HtmlSelect t, Dynamic t (Maybe k))
       -- ^ Returns the element and the currently selected value (if
       -- any).
select attributes initial setValue options = do
  let set = toAttributeValue <$> setValue
  (element, _) <- createSelectElement (toDom attributes) (toAttributeValue initial) set do
    mapM_ createEntry $ maybe options selectInitial initial
  pure (HtmlSelect element, parse <$> Dom._selectElement_value element)
  where selectInitial k = evalState (go k options) False

        -- special handling: always treat value="" as Nothing
        --
        -- without this, using a select with a Text key means that
        -- placeholders are treated as Just "" rather than Nothing,
        -- which is confusing and inconsistent with how value="" is
        -- interpreted by HTML
        parse = \case
          ""   -> Nothing
          text -> fromAttributeValue text

        -- select only the /first/ option with a key that matches
        -- initial
        --
        -- TODO: less fiddly implementation of this logic?
        go k opts = get >>= \case
          True  -> pure opts
          False -> case opts of
            o@(Option _ _ (Just k') _) : rest
              | k == k'   -> (selected o : rest) <$ put True
            o@Option{} : rest ->
              (o :) <$> go k rest
            (Group enabled label groupOpts : rest) -> do
              group' <- Group enabled label <$> go k groupOpts
              rest' <- go k rest
              pure (group' : rest')
            [] -> pure []
{-# INLINABLE select #-}

    -- TODO: support select menus with multiple selection
    --
    -- looks like this will require a fair bit of custom code because
    -- reflex-dom's selectElement API is hardcoded for working with
    -- one value at a time

-- ** Options and Groups

-- | Either an individual option or a group of options.
--
-- Note: A @select@ element can include a mix of options and groups,
-- but groups can only include options themselves—@select@ elements
-- only support one level of nesting. The behavior of nested groups
-- may vary by browser, but it will probably just flatten the groups
-- in the select menu.
data Entry k where
  Option :: Enabled
         -- ^ Users cannot select disabled options and the browser
         -- provides an indication in the UI that the option is
         -- disabled.
         -> Bool
         -- ^ Is this option selected by default?
         --
         -- For single-element select menus, only the /first/ option
         -- marked as selected will get selected.
         -> Maybe k
         -- ^ The value for the option.
         --
         -- 'Nothing' corresponds to @value=""@, which means that the
         -- option does not have a valid value. This is useful for
         -- placeholders; if the @select@ is marked as @required@, a
         -- placeholder will not be a valid value.
         -> Text
         -- ^ The user-facing label for the option.
         -> Entry k

  Group  :: Enabled
         -- ^ If a group is disabled, all of the options in the group
         -- are disabled, regardless of their own enabled/disabled
         -- status.
         -> Text
         -- ^ A user-facing label for the group.
         --
         -- Will be rendered as a non-selectable header in the select
         -- widget.
         -> [Entry k]
         -- ^ The list of options that belong to the group.
         --
         -- Note how a select element can take both groups and
         -- options, but a group can only have options itself—select
         -- elements only support one level of nesting.
         -> Entry k
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

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
-- Passing 'Nothing' for the key means @value=""@ for the
-- option—useful for placeholder options.
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
option :: (AsAttributeValue k, Display k) => k -> Entry k
option key = Option Input.Enabled False (Just key) (display key)

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

-- | Create an @option@ or @optgroup@ element for the given 'Entry'.
createEntry :: forall k m t. (AsAttributeValue k, Dom t m)
            => Entry k
            -> m (Html t)
createEntry = \case
  Option enabled isSelected key label ->
    optionElement enabled isSelected key label
  Group enabled label options -> do
    let attributes =
          [ Input.enabled =: enabled
          , native @'["optgroup"] "label" =: label
          ]
    fst <$> Html.html @"optgroup" attributes do
      forM_ options \ (Option optionEnabled isSelected key optionLabel) ->
        optionElement optionEnabled isSelected key optionLabel
  where optionElement enabled isSelected key label = do
          let attributes =
                [ Input.value                     =: toAttributeValue key
                , boolean @'["option"] "selected" =: isSelected
                , Input.enabled                   =: enabled
                ]
          fst <$> Html.html @"option" attributes (Element.text label)

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
