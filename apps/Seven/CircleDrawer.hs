{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Seven.CircleDrawer where

import           Seven.Attributes (ToAttributes (..))
import           Seven.SVG
import           Seven.Widget

import qualified Data.ByteString  as BS

import           Reflex.Dom

widget :: forall m t. Dom t m => m ()
widget = elClass "div" "circle-drawer" do
  action <- elClass "div" "centered controls" do
    undos <- button "↶"
    redos <- button "↷"
    pure $ leftmost [undos, redos]

  elClass "div" "canvas" do
    rec (canvas, _) <- svg' "svg" $ dyn circles
        let canvasClicks = domEvent Mouseup canvas
        circles <- foldDyn addCircle (pure ()) canvasClicks
    pure ()
  where addCircle point body = body *> circleAt point

-- | Create a "standard" circle centered at the given coordinates.
circleAt :: Dom t m => (Int, Int) -> m ()
circleAt (x, y) = circle c []
  where c = Circle { center = (fromIntegral x, fromIntegral y)
                   , radius = 25
                   }


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
