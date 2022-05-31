{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Seven.Timer where

import           Control.Applicative    (liftA2)
import           Control.Lens           ((<&>))
import           Control.Monad.IO.Class (liftIO)

import           Data.Maybe             (fromMaybe, isJust)
import qualified Data.Text              as Text
import qualified Data.Time              as Time

import qualified Reflex
import qualified Reflex.Dom             as Dom

import           Text.Printf            (printf)

import           UI.Element
import           UI.Main                (Runnable (..), withCss)
import           UI.Widget

-- | A timer with a progress bar and a slider to control the total
-- duration.
widget :: forall m t. Dom t m => m ()
widget = Dom.elClass "div" "timer" do
  now <- liftIO Time.getCurrentTime
  ticks <- Reflex.tickLossy 0.01 now
  time <- Reflex.holdDyn now (Reflex._tickInfo_lastUTC <$> ticks)

  Dom.text "Elapsed time:"
  rec let remaining _ Nothing         = 0
          remaining 0 _               = 1
          remaining total (Just part) = realToFrac (part / total)
      progressBar $ remaining <$> duration <*> elapsed
      output $ renderSeconds . fromMaybe 0 <$> elapsed

      r <- range Reflex.never
      let duration = realToFrac . (* 120) <$> r

      resetClick <- button' buttonLabel (pure Enabled)
      let resetTime = Reflex.tag (Just <$> Reflex.current time) resetClick

      startTime <- Reflex.holdDyn Nothing resetTime
      let elapsed = liftA2 diff time startTime
          buttonLabel = elapsed <&> \ e -> if isJust e then "Reset" else "Start"

          diff :: Time.UTCTime -> Maybe Time.UTCTime -> Maybe Time.NominalDiffTime
          diff t start = Time.diffUTCTime t <$> start

  pure ()
    where renderSeconds t = Text.pack $ printf "%.1f s" (realToFrac t :: Double)

main :: IO ()
main = withCss "css/tasks.css" (Runnable widget)
