module Seven.FlightBooker where

import           Control.Applicative    ((<|>))
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))

import           Data.Text.Display      (Display (..))
import qualified Data.Text.Lazy.Builder as Text
import           Data.Time              (Day)
import qualified Data.Time              as Time

import qualified Reflex
import           Reflex                 (Dynamic)
import qualified Reflex.Dom             as Dom

import qualified Text.Printf            as Text

import           UI.Element
import           UI.Widget

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
widget = Dom.elClass "div" "flight-booker" do
  mode  <- selectEnum @Mode
  there <- dateInput (pure Enabled)
  back  <- dateInput (enabledIf . (== Return) <$> mode)

  let canBook = enableButton <$> mode <*> there <*> back
  bookClick <- button' "Book" canBook

  let trip = toTrip <$> mode <*> there <*> back
  booked <- Reflex.foldDyn (<|>) Nothing (Reflex.tag (Reflex.current trip) bookClick)

  void $ Dom.dyn $ confirmationMessage <$> booked
  where enableButton OneWay Just{} _ = Enabled
        enableButton Return (Just start) (Just end)
          | start <= end = Enabled
        enableButton _ _ _ = Disabled

        toTrip OneWay day _     = OneWay' <$> day
        toTrip Return start end = Return' <$> start <*> end

        confirmationMessage Nothing     = pure ()
        confirmationMessage (Just trip) = output $ pure trip

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
  rec date <- Dom.elDynClass "div" (invalidClass <$> date) do
        readInput @Day today Dom.never enabled
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
