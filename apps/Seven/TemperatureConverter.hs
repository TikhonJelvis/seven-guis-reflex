{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
module Seven.TemperatureConverter where

import           Data.Text   (Text)

import qualified Reflex
import qualified Reflex.Dom  as Dom

import           Text.Printf (printf)

import           UI.Element
import           UI.Widget

-- | The Temperature Converter has a frame with two text fields (°C
-- and °F) that convert automatically.
widget :: Dom t m => m ()
widget = Dom.elClass "div" "converter" $ do
  rec f <- temperature "°F" (-32) updateF
      Dom.text "="
      c <- temperature "°C" 0 updateC
      let updateF = toF <$> Reflex.updated c
          updateC = toC <$> Reflex.updated f
  pure ()
  where toF c = c * (9/5) + 32
        toC f = (f - 32) * (5/9)

-- | A temperature in °C or °F, with a user-friendly Show instance.
newtype Temperature = Temperature { toDouble :: Double }
  deriving newtype (Fractional, Num, Read)

instance Show Temperature where show = printf "%.2f" . toDouble

-- | A widget for entering and displaying a temperature.
--
-- Has a textbox and a label for the units (°C vs °F... etc).
--
-- If the user enters an invalid temperature, sets to 'Nothing'.
temperature :: Dom t m
            => Text
            -- ^ Unit label (eg @"°C"@)
            -> Temperature
            -- ^ Starting temperature.
            -> Reflex.Event t Temperature
            -- ^ Updates to the displayed temperature.
            -> m (Reflex.Dynamic t Temperature)
temperature unit start updates = Dom.elClass "div" "temperature" $ do
  t <- readInput start updates (pure Enabled)
  label unit
  ignoreNothing start t
