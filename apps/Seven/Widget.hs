{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}
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
{-# LANGUAGE ViewPatterns        #-}
-- | Widgets that I use throughout the seven example tasks.
module Seven.Widget where

import           Control.Lens      ((<&>), (^.))
import           Control.Monad     (join, void)
import           Control.Monad.Fix (MonadFix)

import qualified Data.Foldable     as Foldable
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe, listToMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Data.Text.Display (Display)
import qualified Data.Text.Display as Display
import           Data.Vector       (Vector)
import qualified Data.Vector       as Vector

import           Reflex
import           Reflex.Dom

import           Text.Printf       (printf)
import           Text.Read         (readMaybe)

-- * Widgets

-- ** Attributes

-- | Add the given class to the set of attributes. If the class is
-- already present, this will add it again.
withClass :: Text -> Map Text Text -> Map Text Text
withClass = Map.insertWith (\ a b -> a <> " " <> b) "class"

-- ** Outputs

type Dom t m = (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)

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
output' attributes value = elDynAttr "div" (withClass "output" <$> attributes) do
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
  elDynAttr "div" (withClass "bar" . toWidth <$> progress) $ pure ()
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
-- Returns the position as a 'Double' between 0 and 1.
range :: forall a m t. Dom t m => m (Dynamic t Double)
range = do
  e <- inputElement config
  pure $ readValue <$> value e
  where config = def & inputElementConfig_initialValue .~ "0.5"
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
listbox :: forall a k m t. (Ord k, Display a, Dom t m)
        => Dynamic t (Map k a)
        -> m (Dynamic t (Maybe (k, a)))
listbox elements = elClass "div" "listbox" do
  rec selections <- listViewWithKey elements (row selected)
      selected <- foldDyn getSelected Nothing selections

  pure $ zipDynWith toKV selected elements
  where row selected k a = do
          (element, _) <- elDynClass' "div" (isSelected k <$> selected) $
            dynText (Display.display <$> a)

          let select   = Just k  <$ domEvent Click element
              unselect = Nothing <$ domEvent Dblclick element
          pure $ leftmost [unselect, select]

        getSelected map _ =
          join $ listToMaybe $ Map.elems map

        isSelected k (Just k')
          | k == k' = "row selected"
        isSelected _ _ = "row"

        toKV (Just k) map = (k,) <$> Map.lookup k map
        toKV Nothing _    = Nothing

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
