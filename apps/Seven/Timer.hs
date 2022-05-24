{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Seven.Timer where

import           Control.Lens           ((<&>))
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString        as BS
import           Data.Maybe             (fromMaybe, isJust)
import qualified Data.Text              as Text
import qualified Data.Time              as Time

import           Reflex.Dom

import           Text.Printf            (printf)

import           UI.Element
import           UI.Widget

-- | A timer with a progress bar and a slider to control the total
-- duration.
widget :: forall m t. ( MonadIO m
                      , Dom t m
                      , MonadIO (Performable m)
                      , TriggerEvent t m
                      , PerformEvent t m
                      ) => m ()
widget = elClass "div" "timer" do
  now <- liftIO Time.getCurrentTime
  ticks <- tickLossy 0.01 now
  time <- holdDyn now (_tickInfo_lastUTC <$> ticks)

  text "Elapsed time:"
  rec let remaining _ Nothing               = 0
          remaining 0 _                     = 1
          remaining duration (Just elapsed) = realToFrac (elapsed / duration)
      progressBar $ remaining <$> duration <*> elapsed
      output $ renderSeconds . fromMaybe 0 <$> elapsed

      r <- range never
      let duration = realToFrac . (* 120) <$> r

      resetTime <- tag (Just <$> current time) <$> button' buttonLabel (constDyn Enabled)
      startTime <- holdDyn Nothing resetTime
      let elapsed = zipDynWith diff time startTime
          buttonLabel = elapsed <&> \ e -> if isJust e then "Reset" else "Start"

          diff :: Time.UTCTime -> Maybe Time.UTCTime -> Maybe Time.NominalDiffTime
          diff time start = Time.diffUTCTime time <$> start

  pure ()
    where renderSeconds t = Text.pack $ printf "%.1f s" (realToFrac t :: Double)

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget

