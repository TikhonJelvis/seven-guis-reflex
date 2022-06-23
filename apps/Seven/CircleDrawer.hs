module Seven.CircleDrawer where

import           Control.Applicative        (liftA2)
import           Control.Lens               (view, (<&>), (??))
import           Control.Monad              (void)

import           Data.Bool                  (bool)
import           Data.Hashable              (Hashable)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as Text
import           Data.Text.Display          (Display, ShowInstance (..))

import           GHC.Generics               (Generic)

import           Linear                     (V2 (..), _x, _y)

import qualified Reflex
import           Reflex                     (Dynamic, Event, (<@), (<@>))

import qualified Text.Printf                as Text

import           UI.Attributes              (class_)
import           UI.Attributes.AttributeSet ((=:), (==:))
import           UI.Css                     (u)
import           UI.Element                 (Dom, dynText)
import qualified UI.Event                   as Event
import           UI.Event                   (MouseButton (..),
                                             MouseEventResult (..))
import qualified UI.History                 as History
import           UI.History                 (Undos (..))
import qualified UI.Html                    as Html
import qualified UI.Html.Input              as Input
import           UI.Main                    (Runnable (..), withCss)
import           UI.Modal                   (ModalState (..), closed, modal)
import qualified UI.PushMap                 as PushMap
import           UI.PushMap                 (PushMap)
import qualified UI.SVG                     as Svg
import           UI.SVG                     (Svg)
import           UI.SVG.Attributes          (cx, cy, r, stroke, stroke_width)
import           UI.Widget

import           Witherable                 (Filterable (..), catMaybes, (<&?>))

main :: IO ()
main = withCss "css/tasks.css" (Runnable widget)

widget :: forall m t. Dom t m => m ()
widget = void $ Html.div_ [ class_ =: ["circle-drawer"] ] do
  rec Undos { undoActions, redoActions } <-
        snd <$> Html.div_ [ class_ =: ["centered", "controls"] ] do
          History.undoControls modifies

      (canvas, clicked) <- circlesCanvas circles beingModified

      circles <- Reflex.foldDyn doAction mempty $
        Reflex.leftmost [adds, previews, modifies, flipChange <$> undoActions, redoActions]

      let adds = mainClicks canvas <&> \ event -> AddCircle (offset event)
          modifies =
            catMaybes $ changeRadius <$> Reflex.current beingModified <@> setRadius
          previews =
            catMaybes $ previewChange <$> Reflex.current targetCircle
                                      <@> Reflex.updated previewRadius

      beingModified <- Reflex.holdDyn Nothing $
        Reflex.leftmost [Just <$> clicked, Nothing <$ setRadius]

      let targetCircle = liftA2 getCircle beingModified circles
      (setRadius, previewRadius) <-
        circleDialog beingModified $ fmap snd <$> targetCircle

  pure ()
  where previewChange (Just (i, Circle { radius })) newRadius =
          Just $ ChangeRadius i Diff { before = radius, after = newRadius }
        previewChange Nothing _ = Nothing

        changeRadius :: Maybe Int -> (Double, Double) -> Maybe Action
        changeRadius i (old, new) = ChangeRadius <$> i ?? Diff old new

        flipChange :: Action -> Action
        flipChange (ChangeRadius i Diff { before, after }) =
          ChangeRadius i Diff { before = after, after = before }
        flipChange a = a

        getCircle (Just i) circles = (i,) <$> PushMap.lookup i circles
        getCircle Nothing _        = Nothing

        mainClicks = Witherable.filter isMain . Event.domEvent Event.Click
        isMain e = button e == Main

-- | A circle defined by its center and radius.
data Circle = Circle { center :: V2 Double, radius :: Double }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

-- | The SVG element where the circles are rendered.
--
-- Returns:
--
--  * the containing SVG element
--  * an 'Event' that fires when a circle is middle clicked
circlesCanvas :: forall m t. Dom t m
              => Dynamic t (PushMap Circle)
              -- ^ The full set of circles to render.
              -> Dynamic t (Maybe Int)
              -- ^ Which circle, if any, should be highlighted.
              -> m (Svg t, Event t Int)
circlesCanvas circles highlighted = Svg.svg [ class_ =: ["canvas"] ] do
  rawClicks <- selectView highlighted circles (withId svgCircle)
  let clicks = rawClicks <&?> \ (MouseEventResult { button }, i) ->
        [i | button == Auxiliary]
  pure clicks

-- | Render an SVG circle.
--
-- Returns an 'Event' that triggers each time the circle is clicked.
svgCircle :: forall m t. Dom t m
          => Dynamic t Circle
          -> Dynamic t Bool
          -> m (Event t MouseEventResult)
svgCircle c isSelected = do
  element <- Svg.circle
    [ cx ==: u . view _x . center <$> c
    , cy ==: u . view _y . center <$> c
    , r  ==: u . radius <$> c

    , class_ ==: bool [] ["selected"] <$> isSelected

    , stroke       =: "#000"
    , stroke_width =: u 2
    ]
  pure $ Event.domEvent Event.Click element

-- | The dialog that lets us control the radius of a cricle.
--
-- The dialog will be shown as a modal each time the input 'Dynamic'
-- updates to a 'Just', and it will be closed each time the input
-- 'Dynamic' updates to a 'Nothing'.
--
-- The function returns two values:
--
--  * A 'Dynamic' with the radius set for the circle
--
--  * An 'Event' that fires with the final radius when the dialog is
--  * closed.
circleDialog :: forall m t. Dom t m
             => Dynamic t (Maybe Int)
             -- ^ The id of the circle being modified.
             -> Dynamic t (Maybe Circle)
             -- ^ The circle that the dialog controls.
             -> m (Event t (Double, Double), Dynamic t Double)
             -- ^ The 'Event' fires when a modification is saved; the
             -- 'Dynamic' is always up to date with the set radius.
circleDialog beingModified targetCircle = do
  (dialogElement, (old, new)) <- modal showHide [] do
    dynText $ message . center . fromMaybe blank <$> targetCircle

    let oldCircle = Reflex.tagPromptlyDyn targetCircle showHide
        oldRadius = radius <$> catMaybes oldCircle
    (_, setting) <- Input.range [] ((/ maxRadius) <$> oldRadius)

    pure (oldRadius, (maxRadius *) <$> setting)

  old' <- Reflex.holdDyn 0 old
  let both = liftA2 (,) old' new
  pure (Reflex.current both <@ closed dialogElement, new)
  where showHide = Reflex.updated beingModified <&> \case
          Just _  -> ShowModal
          Nothing -> Hide

        maxRadius = 500
        message = Text.pack . Text.printf "Adjust diameter of circle at %s" . show

        blank = Circle { center = V2 0 0, radius = 0 }

-- * Editing Actions

-- | Editing actions we can take in the UI
data Action = AddCircle !(V2 Double)
            -- ^ Add a circle with the standard radius at the given
            -- point.
            | ChangeRadius !Int !(Diff Double)
            --              ↑        ↑
            --             id      radius
            -- ^ Change the radius of the circle with the given id.
  deriving stock (Show, Eq)
  deriving Display via (ShowInstance Action)

-- | An atomic change to a value, recording the value before and after
-- the change.
data Diff a = Diff
  { before :: !a
  , after  :: !a
  }
  deriving stock (Show, Eq)

-- | Execute the action on the given state, if possible.
--
-- Some actions like a 'ChangeRadius' for an id that is not in the map
-- cannot be executed, in which case the input map is returned
-- unchanged.
doAction :: Action -> PushMap Circle -> PushMap Circle
doAction (AddCircle center) circles =
  PushMap.push Circle { center, radius = 50 } circles
doAction (ChangeRadius i radius) circles =
  case PushMap.lookup i circles of
    Just c  -> PushMap.insert i c { radius = after radius } circles
    Nothing -> circles
