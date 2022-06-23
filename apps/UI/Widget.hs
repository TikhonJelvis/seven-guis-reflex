-- | Widgets that I use throughout the seven example tasks.
module UI.Widget where

import           Control.Monad                       (void)
import           Control.Monad.Fix                   (MonadFix)

import           Data.Bool                           (bool)
import qualified Data.Map                            as Map
import qualified Data.Text                           as Text
import           Data.Text.Display                   (Display)
import qualified Data.Text.Display                   as Display

import qualified Reflex
import           Reflex                              (Dynamic, Event, Reflex)
import qualified Reflex.Dom                          as Dom
import           Reflex.Dom                          (DomBuilderSpace)

import           Text.Printf                         (printf)

import           UI.Attributes                       (class_, style)
import           UI.Attributes.AttributeSet.Internal ((=:), (==:))
import           UI.Css.Rules                        (CssRules (..))
import           UI.Element                          (Dom, dynText, text)
import           UI.Event                            (EventResult)
import           UI.Html                             (div_, img, ul)
import           UI.Html.Attributes                  (src)
import           UI.Main                             (Runnable (..), withCss)
import qualified UI.PushMap                          as PushMap
import           UI.PushMap                          (PushMap)

-- * Widgets

_demo :: IO ()
_demo = withCss "css/ui-demo.css" $ Runnable $ void $
  ul [ class_ =: "widget-demo" ]
    [ example "image" $ img [ src =: "https://haskell.org/img/haskell-logo.svg" ] ]
  where example description body = do
          div_ [ class_ =: "label" ] (text description)
          body

-- ** Outputs

-- | A div that displays a dynamically updating value.
--
-- These outputs can all be styled with @div.output@.
output :: forall a m t. (Display a, Dom t m) => Dynamic t a -> m ()
output value = void $ div_ [ class_ =: "output" ] do
  dynText $ Display.display <$> value

    -- TODO: use the progress element instead
-- | A progress bar that fills up left-to-right.
--
-- Progress is a 'Double' clamped between 0 (empty) and 1 (full).
progressBar :: forall m t. Dom t m
            => Dynamic t Double
            -- ^ Progress between 0 and 1. Values < 0 are treated as
            -- 0, values > 1 are treated as 1.
            -> m ()
progressBar progress = void $ div_ [ class_ =: "progress-bar" ] do
  div_ [ class_ =: "bar"
       , style ==: toWidth <$> progress
       ] (pure ())
  where toWidth (toPercent -> p) =
          CssRules [("width", Text.pack $ printf "%.2f%%" p)]
        toPercent p = max 0 (min 1 p) * 100

-- ** Inputs

        -- TODO: replace this with native multiple select?
-- | An interactive box that displays a dynamic sequence of
-- values. One value at a time can be selected.
--
-- The dynamic returned has the index currently selected as well as
-- the value itself.
listbox :: forall a m t. (Display a, Dom t m)
        => Dynamic t (PushMap a)
        -> m (Dynamic t (Maybe Int))
listbox elements = snd <$> div_ [ class_ =: ["listbox"] ] do
  rec selected <- Reflex.holdDyn Nothing =<< selectView selected elements row
  pure selected
  where row :: Int -> Dynamic t a -> Dynamic t Bool -> m (Event t (Maybe Int))
        row k a isSelected = do
          (element, _) <- do
            let attributes =
                  [ class_ =: ["row"]
                  , class_ ==: bool [] ["selected"] <$> isSelected
                  ]
            div_ attributes $ Dom.dynText (Display.display <$> a)

          let selected   = Just k  <$ Dom.domEvent Dom.Click element
              unselected = Nothing <$ Dom.domEvent Dom.Dblclick element
          pure $ Reflex.leftmost [unselected, selected]

-- * Element Interaction

-- | Returns a behavior that is 'True' when the mouse is over the
-- element and 'False' otherwise.
--
-- This uses the @mouseenter@ and @mouseleave@ events under the hood,
-- so it registers the mouse when it's over the element even if the
-- element is covered by a child element.
hovering :: Dom t m
         => Bool
         -- ^ Starting state: is the cursor over the element now?
         --
         -- This is a bit awkward, but figuring out whether the cursor
         -- is over a given element in JavaScript is surpringly
         -- fiddly, so pushing it to the caller seems like the best
         -- option right now...
         -> Dom.Element EventResult (DomBuilderSpace m) t
         -> m (Dynamic t Bool)
hovering start element = Reflex.holdDyn start $ Reflex.leftmost [over, leave]
  where over  = True  <$ Dom.domEvent Dom.Mouseover element
        leave = False <$ Dom.domEvent Dom.Mouseleave element


-- * FRP Utilities

-- | Given a 'Dynamic' that can have invalid values ('Nothing'), this
-- gives a 'Dynamic' that has the last valid ('Just') value.
--
-- The resulting 'Dynamic' will /not/ fire an event when the input
-- changes to 'Nothing'.
ignoreNothing :: forall a m t. (Reflex t, Reflex.MonadHold t m, MonadFix m)
              => a
              -- ^ The initial value to use if the input dynamic has
              -- not had a single valid value.
              -> Dynamic t (Maybe a)
              -- ^ A dynamic that can have invalid values.
              -> m (Dynamic t a)
ignoreNothing initial values = Reflex.foldDynMaybe const initial (Reflex.updated values)

-- | The value from the last time the given event fired, or 'Nothing'
-- if it hasn't fired yet.
lastEvent :: (Reflex t, Reflex.MonadHold t m, MonadFix m)
          => Event t a -> m (Dynamic t (Maybe a))
lastEvent = Reflex.foldDyn (\ a _ -> Just a) Nothing

-- ** Select Views

-- | Manage a set of widgets based on a dynamic collection of
-- values. One widget may be optionally selected.
--
-- Each widget is created by a function that takes:
--
--  * The index of the widget
--  * A 'Dynamic' of the value for the widget
--  * A 'Dynamic' for whether that widget is selected
--
-- and returns the widget itself, as well as a stream of events from
-- that widget.
selectView :: Dom t m
           => Dynamic t (Maybe Int)
           -- ^ Which widget, if any, is selected.
           -> Dynamic t (PushMap a)
           -- ^ The dynamic set of values.
           -> (Int -> Dynamic t a -> Dynamic t Bool -> m (Event t b))
           -- ^ The function to create each widget.
           -> m (Event t b)
           -- ^ An event that fires when any of the widget event
           -- fires. Contains the index of the widget and the value
           -- from the fired event.
selectView selection values child = do
  -- shared between children for performance
  let selectionDemux = Reflex.demux selection
  childEvents <- Reflex.listWithKey (PushMap.toMap <$> values) $ \ i a -> do
    let selected = Reflex.demuxed selectionDemux (Just i)
    fmap (i,) <$> child i a selected
  pure $ snd <$> Reflex.switchPromptlyDyn (Reflex.leftmost . Map.elems <$> childEvents)

-- | Keeps track of an 'Int' id for widgets used in 'selectView'.
withId :: (Monad m, Reflex t)
       => (Dynamic t a -> Dynamic t Bool -> m (Event t b))
       -- ^ A widget function for 'selectView'.
       -> (Int -> Dynamic t a -> Dynamic t Bool -> m (Event t (b, Int)))
       -- ^ A widget-generating function that can be passed into
       -- 'selectView'
withId f i value selected = do
  event <- f value selected
  pure $ (,i) <$> event

