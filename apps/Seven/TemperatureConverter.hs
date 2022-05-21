{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
module Seven.TemperatureConverter where

import           Seven.Element
import           Seven.Widget

import           Control.Monad.Fix (MonadFix)

import           Data.Maybe        (fromJust)
import           Data.Text         (Text)
import qualified Data.Text         as Text

import           Reflex
import           Reflex.Dom

import           Text.Printf       (printf)

-- | The Temperature Converter has a frame with two text fields (°C
-- and °F) that convert automatically.
widget :: Dom t m => m ()
widget = elClass "div" "converter" $ do
  rec f <- temperature "°F" (-32) updateF
      text "="
      c <- temperature "°C" 0 updateC
      let updateF = toF <$> updated c
          updateC = toC <$> updated f
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
            -> Event t Temperature
            -- ^ Updates to the displayed temperature.
            -> m (Dynamic t Temperature)
temperature unit start updates = elClass "div" "temperature" $ do
  t <- readInput start updates (constDyn Enabled)
  label unit
  ignoreNothing start t
