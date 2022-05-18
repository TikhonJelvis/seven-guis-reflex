module Main where

import qualified Seven.Counter              as Counter
import qualified Seven.FlightBooker         as FlightBooker
import qualified Seven.TemperatureConverter as TemperatureConverter

import qualified Data.ByteString            as BS
import           Data.FileEmbed             (embedFile)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text

import           Reflex.Dom                 (mainWidgetWithCss)

import           Text.Printf                (printf)


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css FlightBooker.widget
