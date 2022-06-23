module Seven.Timer where

import           Control.Applicative               (liftA2)
import           Control.Lens                      ((<&>))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)

import           Data.Maybe                        (fromMaybe, isJust)
import qualified Data.Text                         as Text
import qualified Data.Time                         as Time

import qualified Reflex

import           Text.Printf                       (printf)

import           UI.Attributes                     (class_)
import           UI.Attributes.AttributeSet.Reflex ((=:))
import qualified UI.Element                        as Element
import           UI.Element
import qualified UI.Html                           as Html
import qualified UI.Html.Input                     as Input
import           UI.Main                           (Runnable (..), withCss)
import           UI.Widget

-- | A timer with a progress bar and a slider to control the total
-- duration.
widget :: forall m t. Dom t m => m ()
widget = void $ Html.div_ [ class_ =: ["timer"] ] do
  now <- liftIO Time.getCurrentTime
  ticks <- Reflex.tickLossy 0.01 now
  time <- Reflex.holdDyn now (Reflex._tickInfo_lastUTC <$> ticks)

  Element.text "Elapsed time:"
  rec let remaining _ Nothing         = 0
          remaining 0 _               = 1
          remaining total (Just part) = realToFrac (part / total)
      progressBar $ remaining <$> duration <*> elapsed
      output $ renderSeconds . fromMaybe 0 <$> elapsed

      r <- snd <$> Input.range [] Reflex.never
      let duration = realToFrac . (* 120) <$> r

      (_, pressed) <- fst <$> Html.button [] do
        Element.dynText $ elapsed <&> \ e ->
          if isJust e then "Reset" else "Start"
      let resetTime = Reflex.tag (Just <$> Reflex.current time) pressed

      startTime <- Reflex.holdDyn Nothing resetTime
      let elapsed = liftA2 diff time startTime

          diff :: Time.UTCTime -> Maybe Time.UTCTime -> Maybe Time.NominalDiffTime
          diff t start = Time.diffUTCTime t <$> start
  pure ()
    where renderSeconds t = Text.pack $ printf "%.1f s" (realToFrac t :: Double)

main :: IO ()
main = withCss "css/tasks.css" (Runnable widget)
