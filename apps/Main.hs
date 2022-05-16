module Main where

import qualified Data.ByteString      as BS
import           Data.FileEmbed       (embedFile)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

import           Text.Printf          (printf)

import           Reflex.Dom           (mainWidgetWithCss)

import qualified Counter
import qualified FlightBooker
import qualified TemperatureConverter


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css FlightBooker.widget
