{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Seven.CircleDrawer where

import           Seven.Attributes  (ToAttributes (..))
import           Seven.Event
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
    rec (canvas, selected) <- svg' "svg" do
          -- TODO: Hack because selectViewListWithKey can't deal with
          -- nothing being selected...
          let selected' = fromMaybe (-1) <$> selected
              justAdded =
                mapMaybe (\ i -> (,True) <$> i) $ updated $ maxKey <$> circles
          selectedFromSvg <- selectViewListWithKey selected' circles svgCircle
          let setSelected = leftmost [justAdded, selectedFromSvg]
          foldDyn (\ (i, isSelected) _ -> [i | isSelected]) Nothing setSelected

        let canvasClicks = domEvent Mouseup canvas
        circles <- foldDyn pushCircle mempty canvasClicks

    let getSelected selected circles = do
          k <- selected
          Map.lookup k circles
    output $ zipDynWith getSelected selected circles
  where pushCircle :: MouseEventResult -> Map Int Circle -> Map Int Circle
        pushCircle MouseEventResult { offset } circles = case maxKey circles of
          Just maxKey -> Map.insert (maxKey + 1) (standardCircle offset) circles
          Nothing     -> [(0, standardCircle offset)]

        svgCircle :: Int
                  -> Dynamic t Circle
                  -> Dynamic t Bool
                  -> m (Event t Bool)
        svgCircle i circle isSelected = do
          let fillSelect = bool [("fill", "#fff0")] [("fill", "gray")] <$> isSelected
          element <- circleAt circle fillSelect
          updated <$> hovering True element

        maxKey :: Map k a -> Maybe k
        maxKey map
          | Map.null map = Nothing
          | otherwise    = Just $ fst $ Map.findMax map

-- * State

-- | The set of circles currently drawn in the app along with
-- information about which circle is selected.
data Circles = Circles
  { circles  :: Map Int Circle
  , selected :: Int
  }
  deriving stock (Show, Eq)

-- | Render an SVG circle for the given 'Circle'.
circleAt :: Dom t m
         => Dynamic t Circle
         -> Dynamic t (Map Text Text)
         -> m (Element EventResult (DomBuilderSpace m) t)
circleAt c attributes = circle c $ (<> baseAttributes) <$> attributes
  where stroke = def { width = 2 }
        baseAttributes = toAttributes stroke <> [("fill", "none")]

-- | A circle at the given point with the "standard" radius—this is
-- the default circle created when a user clicks, before the radius
-- has been adjusted manually.
standardCircle :: (Int, Int) -> Circle
standardCircle (x, y) =
  Circle { center = (fromIntegral x, fromIntegral y), radius = 50 }


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
