{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module TemperatureConverter where

import           Control.Monad.Fix (MonadFix)

import           Data.Text         (Text)
import qualified Data.Text         as Text

import           Reflex
import           Reflex.Dom

import           Widget

-- | The Temperature Converter has a frame with two text fields (°C
-- and °F) that convert automatically.
widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m) => m ()
widget = elClass "div" "converter" $ do
  rec f <- temperature "°F" (-32) updateF
      text "="
      c <- temperature "°C" 0 updateC
      let updateF = toF <$> updated c
          updateC = toC <$> updated f
  pure ()
  where toF c = c * (9/5) + 32
        toC f = (f - 32) * (5/9)

-- | A widget for entering and displaying a temperature.
--
-- Has a textbox and a label for the units (°C vs °F... etc).
--
-- If the user enters an invalid temperature, sets to 'Nothing'.
temperature :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
            => Text
            -- ^ Unit label (eg @"°C"@)
            -> Double
            -- ^ Starting temperature.
            -> Event t Double
            -- ^ Updates to the displayed temperature.
            -> m (Dynamic t Double)
temperature unit start updates = elClass "div" "temperature" $ do
  t <- readInput start updates
  elClass "div" "label" (text unit)
  pure t
