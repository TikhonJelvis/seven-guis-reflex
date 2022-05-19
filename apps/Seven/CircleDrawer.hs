{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Seven.CircleDrawer where

import           Seven.Attributes (ToAttributes (..))
import           Seven.SVG
import           Seven.Widget

import qualified Data.ByteString  as BS

import           Reflex.Dom

widget :: forall m t. Dom t m => m ()
widget = elClass "div" "circleDrawer" do
  svg "svg" do
    let c = Circle { center = (25, 50), radius = 25 }
    circle c (toAttributes def { width = 4, color = "#36f" })

    let c' = Circle { center = (50, 50), radius = 25 }
    circle c' (toAttributes def { width = 4, color = "#36f" })

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
