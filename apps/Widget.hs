{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Widgets that I use throughout the seven example tasks.
module Widget where

import           Control.Lens      ((^.))
import           Control.Monad.Fix (MonadFix)

import qualified Data.Text         as Text
import           Data.Text.Display (Display)
import qualified Data.Text.Display as Display

import           Text.Read         (readMaybe)

import           Reflex
import           Reflex.Dom

type Dom t m = (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)

-- | A label that displays a dynamically updating value.
--
-- These outputs can all be styled with @div.output@.
output :: (Display a, Dom t m) => Dynamic t a -> m ()
output value = elClass "div" "output" $ do
  dynText $ Display.display <$> value

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
readInput :: (Dom t m, Read a, Show a)
          => a
          -- ^ The initial value to display in the input.
          -> Event t a
          -- ^ A stream of events to change the displayed value. If
          -- you don't want to update the input, use 'mempty'.
          -> m (Dynamic t a)
readInput initial setEvents = do
  input <- inputElement config
  holdDyn initial $ fmapMaybe parse $ _inputElement_input input
  where parse = readMaybe . Text.unpack

        config = def & inputElementConfig_initialValue .~ show' initial
                     & inputElementConfig_setValue .~ (show' <$> setEvents)

        show' = Text.pack . show
