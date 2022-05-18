{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Seven.FlightBooker where

import           Seven.Widget

import           Control.Applicative    ((<|>))
import           Control.Monad          (void)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Data.Text              as Text
import           Data.Text.Display      (Display (..))
import qualified Data.Text.Display      as Display
import qualified Data.Text.Lazy.Builder as Text
import           Data.Time              (Day)
import qualified Data.Time              as Time

import           Reflex
import           Reflex.Dom

import qualified Text.Printf            as Text

-- | A UI with three parts:
--
--  1. A combo box with the options "one-way flight" and "return flight"
--  2. Two text boxes for dates
--  3. A "book" button
--
-- The second text box is only enabled when "return flight" is selected.
--
-- If a text box has an invalid date syntax, it is colored red and the
-- "book" button is disabled.
--
-- If the "return flight" option is on and the second text box has a
-- date before the first text box, the "book" button is disabled.
--
-- Clicking the "book" button displays a message with the flight
-- details chosen (ie "A one-way flight on <date>" or "A return flight
-- from <date> to <date>").
widget :: forall m t. (MonadIO m, Dom t m) => m ()
widget = elClass "div" "flight-booker" do
  mode   <- selectEnum @Mode
  oneWay <- dateInput (constDyn Enabled)
  return <- dateInput (enableIfReturn <$> mode)

  let canBook = enableButton <$> mode <*> oneWay <*> return
  bookClick <- button' "Book" canBook

  let trip = toTrip <$> mode <*> oneWay <*> return
  booked <- foldDyn (<|>) Nothing (tag (current trip) bookClick)

  void $ dyn $ confirmationMessage <$> booked
  where enableIfReturn = \case
          Return -> Enabled
          OneWay -> Disabled

        enableIfJust = \case
          Just _  -> Enabled
          Nothing -> Disabled

        enableButton OneWay Just{} _ = Enabled
        enableButton Return (Just start) (Just end)
          | start <= end = Enabled
        enableButton _ _ _ = Disabled

        toTrip OneWay day _     = OneWay' <$> day
        toTrip Return start end = Return' <$> start <*> end

        confirmationMessage Nothing     = pure ()
        confirmationMessage (Just trip) = label $ Display.display trip

-- | A date input with validation.
--
-- If the user inputs an invalid date, this will contain 'Nothing' and
-- the div containing the date input will have the @invalid@ CSS
-- class.
dateInput :: (Dom t m, MonadIO m)
          => Dynamic t Enabled
          -> m (Dynamic t (Maybe Day))
dateInput enabled = do
  today <- liftIO getLocalDay
  rec date <- elDynClass "div" (invalidClass <$> date) do
        readInput @Day today never enabled
  pure date
  where getLocalDay =
          Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

        invalidClass Nothing = "date invalid"
        invalidClass Just{}  = "date"

-- | The kinds of flights we can book. "One way" requires a single
-- date, "return" requires two dates.
data Mode = OneWay | Return deriving (Eq, Bounded, Enum)

instance Display Mode where
  displayBuilder OneWay = "one-way flight"
  displayBuilder Return = "return flight"

data Trip = OneWay' Day | Return' Day Day deriving (Eq, Show)

instance Display Trip where
  displayBuilder = \case
    OneWay' day       ->
      Text.fromString $ Text.printf "One way trip on %s" (show day)
    Return' start end ->
      Text.fromString $ Text.printf "Return trip from %s to %s" (show start) (show end)
