{-# LANGUAGE QuasiQuotes #-}
-- | Widgets that I use throughout the seven example tasks.
module UI.Widget where

import           Control.Monad              (void)
import           Control.Monad.Fix          (MonadFix)

import           Data.Hashable              (Hashable)
import qualified Data.Map                   as Map
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Display          (Display)
import qualified Data.Text.Display          as Display

import           GHC.Generics               (Generic)

import qualified Reflex
import           Reflex                     (Dynamic, Event, Reflex)
import qualified Reflex.Dom                 as Dom
import           Reflex.Dom                 (DomBuilderSpace)

import           Text.Printf                (printf)
import           Text.URI.QQ                (uri)

import           UI.Attributes              (class_, style)
import           UI.Attributes.AttributeSet ((=:), (==:))
import           UI.Css                     (CssRules (..))
import           UI.Element                 (Dom, dynText, text)
import           UI.Event                   (EventResult)
import           UI.Html                    (div_, img, ul)
import           UI.Html.Attributes         (src)
import           UI.Main                    (Runnable (..), withCss)
import qualified UI.PushMap                 as PushMap
import           UI.PushMap                 (PushMap)
import           UI.Url                     (Url (..))

-- * Widgets

demo :: forall m t. Dom t m => m ()
demo = void $ ul [ class_ =: "widget-demo" ]
  [ example "image" $ img [ src =: Url [uri|https://haskell.org/img/haskell-logo.svg|] ]
  ]
  where example :: Text -> m a -> m a
        example description body = do
          div_ [ class_ =: "label" ] (text description)
          body

main :: IO ()
main = withCss "css/ui-demo.css" (Runnable demo)

-- ** Outputs

-- | A div that displays a dynamically updating value.
--
-- These outputs can all be styled with @div.output@.
output :: forall a m t. (Display a, Dom t m) => Dynamic t a -> m ()
output value = void $ div_ [ class_ =: "output" ] do
  dynText $ Display.display <$> value

    -- TODO: use the progress element instead
-- | A progress bar that fills up left-to-right.
--
-- Progress is a 'Double' clamped between 0 (empty) and 1 (full).
progressBar :: forall m t. Dom t m
            => Dynamic t Double
            -- ^ Progress between 0 and 1. Values < 0 are treated as
            -- 0, values > 1 are treated as 1.
            -> m ()
progressBar progress = void $ div_ [ class_ =: "progress-bar" ] do
  div_ [ class_ =: "bar"
       , style ==: toWidth <$> progress
       ] (pure ())
  where toWidth (toPercent -> p) =
          CssRules [("width", Text.pack $ printf "%.2f%%" p)]
        toPercent p = max 0 (min 1 p) * 100

-- ** Inputs

-- | Input elements can have names, which have several uses like
-- associating labels with specific inputs.
newtype Name = Name { toText :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (IsString, Display)

-- -- | Create a checkbox with the given name.
-- --
-- -- You can add a label for the checkbox using 'labelFor':
-- --
-- -- @
-- -- control = do
-- --   checked <- checkbox "my-control"
-- --   labelFor "my-control" "This is my control:"
-- --   pure checked
-- -- @
-- checkbox :: forall m t. Dom t m
--          => Name
--          -- ^ @name@ of the checkbox, used for linking labels to
--          -- checkboxes.
--          -> Bool
--          -- ^ Initial state: checked ('True') or unchecked ('False').
--          -> Event t Bool
--          -- ^ Explicitly set the state of the checkbox, overriding any
--          -- interactions the user has made.
--          --
--          -- Use 'Reflex.never' if you don't want to override the
--          -- checkbox.
--          -> m (HtmlInput t, Dynamic t Bool)
--          -- ^ Whether the checkbox is checked ('True') or unchecked
--          -- ('False').
-- checkbox name initial overrides = do
--   element <- input (pure attributes)

--   changed :: Event t () <- element `on` "input"
--   checked               <- Reflex.performEvent $ getChecked element <$ changed
--   isChecked             <- Reflex.holdDyn initial checked

--   Reflex.performEvent_ $ setChecked element <$> overrides

--   pure (element, isChecked)
--   where attributes = [("name", toText name) , ("type", "checkbox")]
--                      <> Map.fromList [("checked", "") | initial]

--         getChecked = HTMLInputElement.getChecked . rawHtmlInput
--         setChecked = HTMLInputElement.setChecked . rawHtmlInput

-- -- | An input element that contains a value of some readable type.
-- --
-- -- The 'Read' and 'Show' instances need to be consistent with each
-- -- other: @∀a. read (show a) = a@.
-- --
-- -- Warning: this will __not__ work well for 'Text', 'String' or
-- -- similar types because their 'Read'/'Show' instances require quotes
-- -- and escaping (@"foo\\"@ rather than @foo\@).
-- --
-- -- If the value entered by the user doesn't parse, the dynamic will
-- -- contain 'Nothing'.
-- readInput :: forall a m t. (Dom t m, Read a, Show a)
--           => a
--           -- ^ The initial value to display in the input.
--           -> Event t a
--           -- ^ A stream of events to change the displayed value. If
--           -- you don't want to update the input, use 'never'.
--           -> Dynamic t Enabled
--           -- ^ Whether the input should be enabled or disabled. Use
--           -- @pure Enabled@ to always keep it enabled.
--           -> m (Dynamic t (Maybe a))
-- readInput initial setEvents enabled = do
--   modify <- Dom.dynamicAttributesToModifyAttributes $ toAttributes <$> enabled
--   element <- Dom.inputElement (config modify)
--   Reflex.holdDyn (Just initial) $ read' <$> Dom._inputElement_input element
--   where read' = readMaybe . Text.unpack
--         show' = Text.pack . show

--         config modify =
--           def & Dom.inputElementConfig_initialValue .~ show' initial
--               & Dom.inputElementConfig_setValue .~ (show' <$> setEvents)
--               & Dom.modifyAttributes .~ modify

--         toAttributes Enabled  = Map.empty
--         toAttributes Disabled = Map.fromList [("disabled", "true")]

-- | A drop-down selection menu for an element of an enumerable type.
--
-- The select will show every possible value of the type from
-- 'minBound' to 'maxBound', using the 'Display' instance for
-- user-facing text.
--
-- The initial value selected is 'minBound'.
selectEnum :: forall a m t. (Dom t m, Enum a, Bounded a, Display a)
           => m (Dynamic t a)
selectEnum = do
  (element, _) <- Dom.selectElement config options
  pure $ toEnum . read . Text.unpack <$> Dom._selectElement_value element
  where options = mapM @[] toOption [minBound @a .. maxBound]
        toOption value =
          Dom.elAttr "option" ("value" =: showEnum value) $
            Dom.text (Display.display value)

        config = def & Dom.selectElementConfig_initialValue .~ showEnum (minBound @a)

        showEnum = Text.pack . show . fromEnum

-- -- | An interactive box that displays a dynamic sequence of
-- -- values. One value at a time can be selected.
-- --
-- -- The dynamic returned has the index currently selected as well as
-- -- the value itself.
-- listbox :: forall a m t. (Display a, Dom t m)
--         => Dynamic t (PushMap a)
--         -> m (Dynamic t (Maybe Int))
-- listbox elements = Dom.elClass "div" "listbox" do
--   rec selected <- Reflex.holdDyn Nothing =<< selectView selected elements row
--   pure selected
--   where row k a isSelected = do
--           let class_ = bool "row" "selected row"
--           (element, _) <- Dom.elDynClass' "div" (class_ <$> isSelected) $
--             Dom.dynText (Display.display <$> a)

--           let selected   = Just k  <$ Dom.domEvent Dom.Click element
--               unselected = Nothing <$ Dom.domEvent Dom.Dblclick element
--           pure $ Reflex.leftmost [unselected, selected]

-- * Element Interaction

-- | Returns a behavior that is 'True' when the mouse is over the
-- element and 'False' otherwise.
--
-- This uses the @mouseenter@ and @mouseleave@ events under the hood,
-- so it registers the mouse when it's over the element even if the
-- element is covered by a child element.
hovering :: Dom t m
         => Bool
         -- ^ Starting state: is the cursor over the element now?
         --
         -- This is a bit awkward, but figuring out whether the cursor
         -- is over a given element in JavaScript is surpringly
         -- fiddly, so pushing it to the caller seems like the best
         -- option right now...
         -> Dom.Element EventResult (DomBuilderSpace m) t
         -> m (Dynamic t Bool)
hovering start element = Reflex.holdDyn start $ Reflex.leftmost [over, leave]
  where over  = True  <$ Dom.domEvent Dom.Mouseover element
        leave = False <$ Dom.domEvent Dom.Mouseleave element


-- * FRP Utilities

-- | Given a 'Dynamic' that can have invalid values ('Nothing'), this
-- gives a 'Dynamic' that has the last valid ('Just') value.
--
-- The resulting 'Dynamic' will /not/ fire an event when the input
-- changes to 'Nothing'.
ignoreNothing :: forall a m t. (Reflex t, Reflex.MonadHold t m, MonadFix m)
              => a
              -- ^ The initial value to use if the input dynamic has
              -- not had a single valid value.
              -> Dynamic t (Maybe a)
              -- ^ A dynamic that can have invalid values.
              -> m (Dynamic t a)
ignoreNothing initial values = Reflex.foldDynMaybe const initial (Reflex.updated values)

-- | The value from the last time the given event fired, or 'Nothing'
-- if it hasn't fired yet.
lastEvent :: (Reflex t, Reflex.MonadHold t m, MonadFix m)
          => Event t a -> m (Dynamic t (Maybe a))
lastEvent = Reflex.foldDyn (\ a _ -> Just a) Nothing

-- ** Select Views

-- | Manage a set of widgets based on a dynamic collection of
-- values. One widget may be optionally selected.
--
-- Each widget is created by a function that takes:
--
--  * The index of the widget
--  * A 'Dynamic' of the value for the widget
--  * A 'Dynamic' for whether that widget is selected
--
-- and returns the widget itself, as well as a stream of events from
-- that widget.
selectView :: Dom t m
           => Dynamic t (Maybe Int)
           -- ^ Which widget, if any, is selected.
           -> Dynamic t (PushMap a)
           -- ^ The dynamic set of values.
           -> (Int -> Dynamic t a -> Dynamic t Bool -> m (Event t b))
           -- ^ The function to create each widget.
           -> m (Event t b)
           -- ^ An event that fires when any of the widget event
           -- fires. Contains the index of the widget and the value
           -- from the fired event.
selectView selection values child = do
  -- shared between children for performance
  let selectionDemux = Reflex.demux selection
  childEvents <- Reflex.listWithKey (PushMap.toMap <$> values) $ \ i a -> do
    let selected = Reflex.demuxed selectionDemux (Just i)
    fmap (i,) <$> child i a selected
  pure $ snd <$> Reflex.switchPromptlyDyn (Reflex.leftmost . Map.elems <$> childEvents)

-- | Keeps track of an 'Int' id for widgets used in 'selectView'.
withId :: (Monad m, Reflex t)
       => (Dynamic t a -> Dynamic t Bool -> m (Event t b))
       -- ^ A widget function for 'selectView'.
       -> (Int -> Dynamic t a -> Dynamic t Bool -> m (Event t (b, Int)))
       -- ^ A widget-generating function that can be passed into
       -- 'selectView'
withId f i value selected = do
  event <- f value selected
  pure $ (,i) <$> event

