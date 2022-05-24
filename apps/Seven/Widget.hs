{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Widgets that I use throughout the seven example tasks.
module Seven.Widget where

import           Seven.Element
import           Seven.Event
import qualified Seven.PushMap     as PushMap
import           Seven.PushMap     (PushMap)

import           Control.Lens      ((<&>), (^.))
import           Control.Monad     (join, void)
import           Control.Monad.Fix (MonadFix)

import           Data.Bool         (bool)
import qualified Data.Foldable     as Foldable
import           Data.IntMap       (IntMap)
import           Data.IntSet       (IntSet)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe, listToMaybe)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Data.Text.Display (Display)
import qualified Data.Text.Display as Display
import           Data.Vector       (Vector)
import qualified Data.Vector       as Vector

import           GHC.Exts          (IsList (..))

import           Reflex
import           Reflex.Dom        hiding (EventResult, elDynAttr')

import           Text.Printf       (printf)
import           Text.Read         (readMaybe)

-- * Widgets

-- ** Attributes

-- *** CSS Classes

-- | Does the given set of attributes have a class with the given
-- name?
--
-- >>> hasClass "selected" [("class", "my selected widget")]
-- True
--
-- >>> hasClass "selected" [("class", "my widget")]
-- False
--
-- >>> hasClass "selected" []
-- False
hasClass :: Text -> Map Text Text -> Bool
hasClass class_ attributes = case Map.lookup "class" attributes of
  Just attribute -> class_ `elem` classes attribute
  Nothing        -> False

-- | Add a class to the given set of attributes.
--
-- This function will always normalize existing class attribute by
-- dropping extra whitespace, sorting the class names and dropping any
-- duplicates.
--
-- >>> addClass "selected" []
-- fromList [("class","selected")]
--
-- >>> addClass "selected" [("class","selected   widget   selected")]
-- fromList [("class","selected widget")]
--
-- >>> addClass "selected" [("class", "my widget")]
-- fromList [("class","my selected widget")]
--
addClass :: Text -> Map Text Text -> Map Text Text
addClass = Map.insertWith add "class"
  where add class_ = joinClasses . Set.insert class_ . classes

-- | Add or remove a class based on a boolean: 'True' adds the class,
-- 'False' removes it.
--
-- >>> setClass "selected" True [("class", "my widget")]
-- fromList [("class","my selected widget")]
--
-- >>> setClass "selected" False [("class", "my selected widget")]
-- fromList [("class","my widget")]
--
setClass :: Text -> Bool -> Map Text Text -> Map Text Text
setClass class_ = bool (removeClass class_) (addClass class_)

-- | Remove the class from the given set of attributes if it is
-- present.
--
-- This function will always normalize existing class attribute by
-- dropping extra whitespace, sorting the class names and dropping any
-- duplicates.
--
-- If the resulting class attribute would be empty, the attribute is
-- removed from the set of attributes altogether.
--
-- >>> removeClass "selected" [("class", "my selected widget")]
-- fromList [("class","my widget")]
--
-- >>> removeClass "selected" [("class", "my widget")]
-- fromList [("class","my widget")]
--
-- >>> removeClass "selected" [("class","")]
-- fromList []
--
-- >>> removeClass "selected" []
-- fromList []
--
removeClass :: Text -> Map Text Text -> Map Text Text
removeClass class_ = Map.update remove "class"
  where remove attribute =
          case joinClasses $ Set.delete class_ $ classes attribute of
            ""  -> Nothing
            new -> Just new

-- | Given an entry for a @"class"@ attribute, parse out the classes
-- set by the attribute.
--
-- >>> classes "foo"
-- fromList ["foo"]
--
-- >>> classes "foo bar baz"
-- fromList ["bar","baz","foo"]
--
-- >>> classes "  foo   bar  "
-- fromList ["bar","foo"]
--
-- Note that classes can have non-ASCII whitespace as part of the
-- name:
--
-- >>> classes "bar  foo   "
-- fromList ["bar\8239\8239foo","\8239"]
--
classes :: Text
        -- ^ Class attribute
        -> Set Text
classes = Set.delete "" . Set.fromList . Text.split isHtmlWhitespace

-- | Combine a set of classes into a single attribute value, with each
-- class name separated by a single space.
--
-- >>> joinClasses (Set.fromList ["widget", "my", "selected"])
-- "my selected widget"
--
-- >>> joinClasses []
-- ""
--
joinClasses :: Foldable f => f Text -> Text
joinClasses = Text.intercalate " " . Foldable.toList

-- | Is the character an HTML whitespace character?
--
-- HTML uses ASCII whitespace characters to separate class names/etc,
-- but does not treat non-ASCII whitespace specially in those cases.
--
-- See [the definition of ASCII
-- whitespace](https://infra.spec.whatwg.org/#ascii-whitespace) for
-- details.
--
-- >>> isHtmlWhitespace ' '
-- True
--
-- >>> isHtmlWhitespace 'a'
-- False
--
-- >>> isHtmlWhitespace '\8239' -- narrow no-break space
-- False
--
isHtmlWhitespace :: Char -> Bool
isHtmlWhitespace c = c `elem` htmlWhitespace

-- | The set of ASCII whitespace characters as defined by the HTML
-- standard.
--
-- HTML uses ASCII whitespace characters to separate class names/etc,
-- but does not treat non-ASCII whitespace specially in those cases.
--
-- See [the definition of ASCII
-- whitespace](https://infra.spec.whatwg.org/#ascii-whitespace) for
-- details.
--
-- >>> htmlWhitespace
-- fromList "\t\n\f\r "
--
htmlWhitespace :: Set Char
htmlWhitespace = [' ', '\t', '\n', '\f', '\r']

-- ** Outputs

-- | A static label with some text. Has the CSS class @label@.
label :: forall m t. DomBuilder t m => Text -> m ()
label = elClass "div" "label" . text

-- | A label that displays a dynamically updating value.
--
-- These outputs can all be styled with @div.output@.
output :: forall a m t. (Display a, Dom t m) => Dynamic t a -> m ()
output value = elClass "div" "output" do
  dynText $ Display.display <$> value

-- | A label displaying the given value. Has the CSS class @output@.
--
-- You can provide dynamic attributes.
output' :: forall a m t. (Display a, Dom t m)
        => Dynamic t (Map Text Text)
        -- ^ Dynamically changing attributes.
        -> Dynamic t a
        -- ^ Dynamically changing value to display.
        -> m ()
output' attributes value = elDynAttr "div" (addClass "output" <$> attributes) do
  dynText $ Display.display <$> value

-- | A progress bar that fills up left-to-right.
--
-- Progress is a 'Double' clamped between 0 (empty) and 1 (full).
progressBar :: forall m t. Dom t m
            => Dynamic t Double
            -- ^ Progress between 0 and 1. Values < 0 are treated as
            -- 0, values > 1 are treated as 1.
            -> m ()
progressBar progress = elClass "div" "progress-bar" do
  elDynAttr "div" (addClass "bar" . toWidth <$> progress) $ pure ()
  where toWidth :: Double -> Map Text Text
        toWidth (toPercent -> p) =
          [("style", Text.pack $ printf "width: %.2f%%" p)]
        toPercent p = max 0 (min 1 p) * 100

-- ** Inputs

-- | Whether an input element is enabled or disabled.
--
-- A disabled element should not accept user input and should have
-- some visual indication that it is disabled.
data Enabled = Enabled | Disabled deriving (Show, Eq)

-- | 'Enabled' if 'True', 'Disabled' if 'False'.
enabledIf :: Bool -> Enabled
enabledIf = bool Disabled Enabled

-- | Set the given element attributes to reflect the given 'Enabled'
-- state.
--
-- Other attributes are left unchanged.
setEnabled :: Map Text Text -> Enabled -> Map Text Text
setEnabled attributes Enabled  = Map.delete "disabled" attributes
setEnabled attributes Disabled = Map.insert "disabled" "true" attributes

-- | A button with the given label that can be enabled or disabled,
-- with a label that can change dynamically.
--
-- Returns a stream of button press events.
button' :: forall a m t. (Dom t m)
        => Dynamic t Text
        -- ^ The button label.
        -> Dynamic t Enabled
        -- ^ Whether the button is enabled or disabled.
        -> m (Event t ())
button' label enabled = do
  (e, _) <- elDynAttr' "button" attrs $ dynText label
  pure $ void $ domEvent Click e
  where attrs = enabled <&> \case
          Enabled  -> []
          Disabled -> [("disabled", "true")]

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
          -- @constDyn Enabled@ to always keep it enabled.
          -> m (Dynamic t (Maybe a))
readInput initial setEvents enabled = do
  modify <- dynamicAttributesToModifyAttributes $ toAttributes <$> enabled
  input <- inputElement (config modify)
  holdDyn (Just initial) $ read' <$> _inputElement_input input
  where read' = readMaybe . Text.unpack
        show' = Text.pack . show

        config modify =
          def & inputElementConfig_initialValue .~ show' initial
              & inputElementConfig_setValue .~ (show' <$> setEvents)
              & modifyAttributes .~ modify

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
  (select, _) <- selectElement config options
  pure $ toEnum . read . Text.unpack <$> _selectElement_value select
  where options = mapM @[] toOption [minBound @a .. maxBound]
        toOption value =
          elAttr "option" ("value" =: showEnum value) $
            text (Display.display value)

        config = def & selectElementConfig_initialValue .~ showEnum (minBound @a)

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
-- range never
-- @
--
-- Returns the position as a 'Double' between 0 and 1.
range :: forall a m t. Dom t m => Event t Double -> m (Dynamic t Double)
range (fmap (Text.pack . show) -> setEvents) = do
  e <- inputElement config
  pure $ readValue <$> value e
  where config = def & inputElementConfig_initialValue .~ "0.5"
                     & inputElementConfig_setValue .~ setEvents
                     & initialAttributes .~ [ ("type", "range")
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
listbox elements = elClass "div" "listbox" do
  rec selected <- holdDyn Nothing =<< selectView selected elements row
  pure selected
  where row k a selected = do
          let isSelected is = if is then "row selected" else "row"
          (element, _) <- elDynClass' "div" (isSelected <$> selected) $
            dynText (Display.display <$> a)

          let select   = Just k  <$ domEvent Click element
              unselect = Nothing <$ domEvent Dblclick element
          pure $ leftmost [unselect, select]

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
         -> Element EventResult (DomBuilderSpace m) t
         -> m (Dynamic t Bool)
hovering start element = holdDyn start $ leftmost [over, leave]
  where over  = True  <$ domEvent Mouseover element
        leave = False <$ domEvent Mouseleave element


-- * FRP Utilities

-- | Given a 'Dynamic' that can have invalid values ('Nothing'), this
-- gives a 'Dynamic' that has the last valid ('Just') value.
--
-- The resulting 'Dynamic' will /not/ fire an event when the input
-- changes to 'Nothing'.
ignoreNothing :: forall a m t. (Reflex t, MonadHold t m, MonadFix m)
              => a
              -- ^ The initial value to use if the input dynamic has
              -- not had a single valid value.
              -> Dynamic t (Maybe a)
              -- ^ A dynamic that can have invalid values.
              -> m (Dynamic t a)
ignoreNothing initial input = foldDynMaybe const initial (updated input)

-- | The value from the last time the given event fired, or 'Nothing'
-- if it hasn't fired yet.
lastEvent :: (Reflex t, MonadHold t m, MonadFix m)
          => Event t a -> m (Dynamic t (Maybe a))
lastEvent = foldDyn (\ a _ -> Just a) Nothing

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
selectView :: (Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m, Reflex t)
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
  let selectionDemux = demux selection
  childEvents <- listWithKey (PushMap.toMap <$> values) $ \ i a -> do
    let selected = demuxed selectionDemux (Just i)
    fmap (i,) <$> child i a selected
  pure $ snd <$> switchPromptlyDyn (leftmost . Map.elems <$> childEvents)

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

