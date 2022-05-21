{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Seven.CircleDrawer where

import           Seven.Attributes  (ToAttributes (..))
import           Seven.Element
import           Seven.Event
import qualified Seven.PushMap     as PushMap
import           Seven.SVG
import           Seven.Widget

import           Control.Lens      ((<&>), (??))
import           Control.Monad     (void)

import           Data.Bool         (bool)
import qualified Data.ByteString   as BS
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe)
import           Data.Text         (Text)
import           Data.Text.Display (Display)

import qualified Reflex.Dom        as Reflex hiding (EventResult)
import           Reflex.Dom        hiding (EventResult, button)

import           Witherable        (catMaybes)

widget :: forall m t. Dom t m => m ()
widget = elClass "div" "circle-drawer" do
  action <- elClass "div" "centered controls" do
    undos <- Reflex.button "↶"
    redos <- Reflex.button "↷"
    pure $ leftmost [undos, redos]

  elClass "div" "canvas" do
    rec circles <- foldDyn pushCircle mempty $ domEvent Click canvas
        (canvas, selected) <- svg' "svg" do
          let justAdded = updated $ PushMap.maxKey <$> circles
          selectedFromSvg <- selectView selected circles svgCircle
          holdDyn Nothing $ leftmost [justAdded, selectedFromSvg]

    let getSelected selected circles = do
          k <- selected
          PushMap.lookup k circles
    output $ zipDynWith getSelected selected circles
  where pushCircle MouseEventResult { offset = (x, y) } =
          PushMap.push Circle { center = (fromIntegral x, fromIntegral y), radius = 50 }

        svgCircle :: Int -> Dynamic t Circle -> Dynamic t Bool -> m (Event t (Maybe Int))
        svgCircle i circle isSelected = do
          let fillSelect = bool [("fill", "#fff0")] [("fill", "gray")] <$> isSelected
          element <- circleAt circle fillSelect
          isHovered <- hovering True element
          pure $ updated $ isHovered <&> \ hovered ->
            if hovered then Just i else Nothing

        circleAt c attributes = circle c $ withDefaults <$> attributes
        withDefaults attributes = attributes <> toAttributes def { width = 2 }

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
