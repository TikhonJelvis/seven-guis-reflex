module Seven.FlightBooker where

import           Control.Applicative               ((<|>))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO)

import           Data.Text.Display                 (Display (..))
import qualified Data.Text.Lazy.Builder            as Text
import           Data.Time                         (Day)

import qualified Reflex

import qualified Text.Printf                       as Text

import           UI.Attributes                     (class_)
import           UI.Attributes.AttributeSet.Reflex ((=:), (==:))
import qualified UI.Element                        as Element
import           UI.Element
import qualified UI.Html                           as Html
import           UI.Html                           (Button (..))
import qualified UI.Html.Input                     as Input
import           UI.Html.Input                     (Enabled (..), enabled,
                                                    enabledIf)
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
widget = void $ Html.div_ [ class_ =: ["flight-booker"] ] do
  mode  <- selectEnum @Mode
  there <- snd <$> Input.date [] Reflex.never
  back  <- snd <$>
    Input.date [ enabled ==: enabledIf . (== Return) <$> mode ] Reflex.never

  let canBook = enableButton <$> mode <*> there <*> back
  Button { pressed } <- Html.button' "Book" [ enabled ==: canBook ]

  let trip = toTrip <$> mode <*> there <*> back
  booked <- Reflex.foldDyn (<|>) Nothing (Reflex.tag (Reflex.current trip) pressed)

  void $ Element.dyn $ confirmationMessage <$> booked
  where enableButton OneWay Just{} _ = Enabled
        enableButton Return (Just start) (Just end)
          | start <= end = Enabled
        enableButton _ _ _ = Disabled

        toTrip OneWay day _     = OneWay' <$> day
        toTrip Return start end = Return' <$> start <*> end

        confirmationMessage Nothing     = pure ()
        confirmationMessage (Just trip) = output $ pure trip

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
