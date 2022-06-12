{-# LANGUAGE QuasiQuotes #-}
-- | Widgets that I use throughout the seven example tasks.
module UI.Widget where

import           Control.Lens               ((&), (.~), (<&>))
import           Control.Monad              (void)
import           Control.Monad.Fix          (MonadFix)

import           Data.Bool                  (bool)
import           Data.Default.Class         (def)
import           Data.Hashable              (Hashable)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Display          (Display)
import qualified Data.Text.Display          as Display

import           GHC.Generics               (Generic)

import qualified GHCJS.DOM.HTMLInputElement as HTMLInputElement

import qualified Reflex
import           Reflex                     (Dynamic, Event, Reflex)
import qualified Reflex.Dom                 as Dom
import           Reflex.Dom                 (DomBuilder, DomBuilderSpace, (=:))

import           Text.Printf                (printf)
import           Text.Read                  (readMaybe)
import qualified Text.URI                   as URI
import           Text.URI                   (URI)
import           Text.URI.QQ                (uri)

import           UI.Element                 (Dom, Html, HtmlInput, el', elAttr',
                                             elDynAttr', input)
import           UI.Element.IsElement       (rawHtmlInput)
import           UI.Event                   (EventResult, on)
import           UI.Main                    (Runnable (..), withCss)
import qualified UI.PushMap                 as PushMap
import           UI.PushMap                 (PushMap)

-- * Widgets

demo :: forall m t. Dom t m => m ()
demo = void $ ul (pure [("class", "widget-demo")])
  [ example "image" $ image [uri|https://haskell.org/img/haskell-logo.svg|]
  ]
  where example description body = do
          label description
          body

main :: IO ()
main = withCss "css/ui-demo.css" (Runnable demo)

-- ** Structure

-- | An ordered list, with the given widgets as list items.
--
-- Each item is automatically wrapped in an @li@ element.
ol :: forall a m t. Dom t m
   => Dynamic t (Map Text Text)
   -- ^ Attributes
   -> [m a]
   -- ^ List items
   -> m (Html t, [a])
ol attributes = elDynAttr' "ol" attributes . mapM (Dom.el "li")

-- | An unordered list, with the given widgets as list items.
--
-- Each item is automatically wrapped in an @li@ element.
ul :: forall a m t. Dom t m
   => Dynamic t (Map Text Text)
   -- ^ Attributes
   -> [m a]
   -- ^ List items
   -> m (Html t, [a])
ul attributes = elDynAttr' "ul" attributes . mapM (Dom.el "li")

-- ** Outputs

-- | A static label with some text. Uses the HTML @label@ element.
label :: forall m t. Dom t m => Text -> m (Html t)
label text = fst <$> el' "label" (Dom.text text)

-- | A label for a named element. Labels can be associated with:
--
--  * @button@
--  * @input@
--  * @meter@
--  * @output@
--  * @progress@
--  * @select@
--  * @textarea@
--
labelFor :: forall m t. Dom t m => Name -> Text -> m (Html t)
labelFor name text =
  fst <$> elAttr' "label" [("for", toText name)] (Dom.text text)

-- | An image with a dynamic @src@ attribute.
img :: forall m t. Dom t m
    => Dynamic t URI
    -- ^ src
    -> Dynamic t (Map Text Text)
    -- ^ other attributes (src will be overridden)
    -> m (Html t)
img src attributes =
  fst <$> elDynAttr' "img" (setSrc <$> src <*> attributes) (pure ())
  where setSrc = Map.insert "src" . URI.render

-- | A static image.
--
-- __Example__
--
-- @
-- image [uri|https://haskell.org/img/haskell-logo.svg|]
-- @
image :: forall m t. Dom t m
      => URI
      -- ^ src
      -> m (Html t)
image src = img (pure src) (pure [])

-- | A label that displays a dynamically updating value.
--
-- These outputs can all be styled with @div.output@.
output :: forall a m t. (Display a, Dom t m) => Dynamic t a -> m ()
output value = Dom.elClass "div" "output" do
  Dom.dynText $ Display.display <$> value

-- | A label displaying the given value. Has the CSS class @output@.
--
-- You can provide dynamic attributes.
output' :: forall a m t. (Display a, Dom t m)
        => Dynamic t (Map Text Text)
        -- ^ Dynamically changing attributes.
        -> Dynamic t a
        -- ^ Dynamically changing value to display.
        -> m ()
output' attributes value = Dom.elDynAttr "div" (addClass "output" <$> attributes) do
  Dom.dynText $ Display.display <$> value

-- | A progress bar that fills up left-to-right.
--
-- Progress is a 'Double' clamped between 0 (empty) and 1 (full).
progressBar :: forall m t. Dom t m
            => Dynamic t Double
            -- ^ Progress between 0 and 1. Values < 0 are treated as
            -- 0, values > 1 are treated as 1.
            -> m ()
progressBar progress = Dom.elClass "div" "progress-bar" do
  Dom.elDynAttr "div" (addClass "bar" . toWidth <$> progress) $ pure ()
  where toWidth :: Double -> Map Text Text
        toWidth (toPercent -> p) =
          [("style", Text.pack $ printf "width: %.2f%%" p)]
        toPercent p = max 0 (min 1 p) * 100

-- ** Inputs

-- | Input elements can have names, which have several uses like
-- associating labels with specific inputs.
newtype Name = Name { toText :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (IsString, Display)

-- | Whether an input element is enabled or disabled.
--
-- A disabled element should not accept user input and should have
-- some visual indication that it is disabled.
data Enabled = Enabled
             | Disabled
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | 'Enabled' if 'True', 'Disabled' if 'False'.
enabledIf :: Bool -> Enabled
enabledIf = bool Disabled Enabled

-- | Set the given element attributes to reflect the given 'Enabled'
-- state.
--
-- Other attributes are left unchanged.
setEnabled :: Enabled -> Map Text Text -> Map Text Text
setEnabled Enabled attributes  = Map.delete "disabled" attributes
setEnabled Disabled attributes = Map.insert "disabled" "true" attributes

-- | A button with the given label that can be enabled or disabled,
-- with a label that can change dynamically.
--
-- Returns a stream of button press events.
button' :: forall m t. (Dom t m)
        => Dynamic t Text
        -- ^ The button label.
        -> Dynamic t Enabled
        -- ^ Whether the button is enabled or disabled.
        -> m (Event t ())
button' l enabled = do
  (e, _) <- elDynAttr' "button" attrs $ Dom.dynText l
  pure $ void $ Dom.domEvent Dom.Click e
  where attrs = enabled <&> \case
          Enabled  -> []
          Disabled -> [("disabled", "true")]

-- | Create a checkbox with the given name.
--
-- You can add a label for the checkbox using 'labelFor':
--
-- @
-- control = do
--   checked <- checkbox "my-control"
--   labelFor "my-control" "This is my control:"
--   pure checked
-- @
checkbox :: forall m t. Dom t m
         => Name
         -- ^ @name@ of the checkbox, used for linking labels to
         -- checkboxes.
         -> Bool
         -- ^ Initial state: checked ('True') or unchecked ('False').
         -> Event t Bool
         -- ^ Explicitly set the state of the checkbox, overriding any
         -- interactions the user has made.
         --
         -- Use 'Reflex.never' if you don't want to override the
         -- checkbox.
         -> m (HtmlInput t, Dynamic t Bool)
         -- ^ Whether the checkbox is checked ('True') or unchecked
         -- ('False').
checkbox name initial overrides = do
  element <- input (pure attributes)

  changed :: Event t () <- element `on` "input"
  checked               <- Reflex.performEvent $ getChecked element <$ changed
  isChecked             <- Reflex.holdDyn initial checked

  Reflex.performEvent_ $ setChecked element <$> overrides

  pure (element, isChecked)
  where attributes = [("name", toText name) , ("type", "checkbox")]
                     <> Map.fromList [("checked", "") | initial]

        getChecked = HTMLInputElement.getChecked . rawHtmlInput
        setChecked = HTMLInputElement.setChecked . rawHtmlInput

-- | An input element that contains a value of some readable type.
--
-- The 'Read' and 'Show' instances need to be consistent with each
-- other: @∀a. read (show a) = a@.
--
-- Warning: this will __not__ work well for 'Text', 'String' or
-- similar types because their 'Read'/'Show' instances require quotes
-- and escaping (@"foo\\"@ rather than @foo\@).
--
-- If the value entered by the user doesn't parse, the dynamic will
-- contain 'Nothing'.
readInput :: forall a m t. (Dom t m, Read a, Show a)
          => a
          -- ^ The initial value to display in the input.
          -> Event t a
          -- ^ A stream of events to change the displayed value. If
          -- you don't want to update the input, use 'never'.
          -> Dynamic t Enabled
          -- ^ Whether the input should be enabled or disabled. Use
          -- @pure Enabled@ to always keep it enabled.
          -> m (Dynamic t (Maybe a))
readInput initial setEvents enabled = do
  modify <- Dom.dynamicAttributesToModifyAttributes $ toAttributes <$> enabled
  element <- Dom.inputElement (config modify)
  Reflex.holdDyn (Just initial) $ read' <$> Dom._inputElement_input element
  where read' = readMaybe . Text.unpack
        show' = Text.pack . show

        config modify =
          def & Dom.inputElementConfig_initialValue .~ show' initial
              & Dom.inputElementConfig_setValue .~ (show' <$> setEvents)
              & Dom.modifyAttributes .~ modify

        toAttributes Enabled  = Map.empty
        toAttributes Disabled = Map.fromList [("disabled", "true")]

-- | A drop-down selection menu for an element of an enumerable type.
--
-- The select will show every possible value of the type from
-- 'minBound' to 'maxBound', using the 'Display' instance for
-- user-facing text.
--
-- The initial value selected is 'minBound'.
selectEnum :: forall a m t. (DomBuilder t m, Enum a, Bounded a, Display a)
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

-- | A range input element (a slider).
--
-- The argument is an 'Event' that sets the slider to the given
-- value. Values < 0 will be clamped to 0; values > 1 will be clamped
-- to 1.
--
-- If you do not want to set the element, you can use
--
-- @
-- range Reflex.never
-- @
--
-- Returns the position as a 'Double' between 0 and 1.
range :: forall m t. Dom t m => Event t Double -> m (Dynamic t Double)
range (fmap (Text.pack . show) -> setEvents) = do
  e <- Dom.inputElement config
  pure $ readValue <$> Dom.value e
  where config = def & Dom.inputElementConfig_initialValue .~ "0.5"
                     & Dom.inputElementConfig_setValue .~ setEvents
                     & Dom.initialAttributes .~ [ ("type", "range")
                                                , ("min", "0.0")
                                                , ("max", "1.0")
                                                , ("step", "any") ]

        -- This should only be Nothing if somebody is manually
        -- screwing around with the DOM or something...
        readValue = fromMaybe 0 . readMaybe . Text.unpack

-- | An interactive box that displays a dynamic sequence of
-- values. One value at a time can be selected.
--
-- The dynamic returned has the index currently selected as well as
-- the value itself.
listbox :: forall a m t. (Display a, Dom t m)
        => Dynamic t (PushMap a)
        -> m (Dynamic t (Maybe Int))
listbox elements = Dom.elClass "div" "listbox" do
  rec selected <- Reflex.holdDyn Nothing =<< selectView selected elements row
  pure selected
  where row k a isSelected = do
          let class_ = bool "row" "selected row"
          (element, _) <- Dom.elDynClass' "div" (class_ <$> isSelected) $
            Dom.dynText (Display.display <$> a)

          let selected   = Just k  <$ Dom.domEvent Dom.Click element
              unselected = Nothing <$ Dom.domEvent Dom.Dblclick element
          pure $ Reflex.leftmost [unselected, selected]

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

