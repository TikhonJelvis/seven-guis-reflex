module UI.Drag where

import           Control.Applicative               (liftA2)
import           Control.Monad                     (void)

import           Data.Default.Class                (Default (..))
import           Data.Foldable                     (toList)
import           Data.Functor                      ((<&>))
import           Data.Maybe                        (fromMaybe, isJust)
import           Data.Text                         (Text)

import           GHC.Generics                      (Generic)

import qualified GHCJS.DOM.Document                as Document
import qualified GHCJS.DOM.Element                 as Element

import           Linear                            (V2 (..), _x, _y, distance,
                                                    project, unit)

import qualified Reflex
import           Reflex                            (Dynamic, Event)
import qualified Reflex.Dom                        as Dom

import           UI.Attributes.AttributeSet.Reflex (AttributeSet, (=:))
import           UI.Element                        (Dom)
import           UI.Element.IsElement              (rawElement)
import           UI.Event                          (Modifier (Shift),
                                                    MouseButton (..),
                                                    MouseEventResult (..),
                                                    button, client, mouseEvent,
                                                    on, performJs)
import qualified UI.Html                           as Html
import           UI.Html                           (Html)
import           UI.Main                           (Runnable (..), withCss)
import           UI.Style                          (Angle (..),
                                                    getComputedProperty,
                                                    setProperty)

import qualified Witherable

demo :: forall m t. Dom t m => m ()
demo = void do
  Html.ul [ class_ =: ["drag-demo"] ]
    [ example "Follow cursor exactly" def translate
    , example "Follow with transition" def withTransition
    , example "Horizontal only" def xOnly
    , example "Vertical only" def yOnly
    , example "Snap to 50px grid" def (snapTo 50)
    , example "Rotate" def rotateByDistance
    , example "Scale" def scaleByDistance
    , example "Middle mouse button while holding @shift@" shiftConfig translate

    , snapBack
    , dragHandle
    , enableDisable
    ]
  dragAnywhere
  where withTransition p = addClass "smooth-drag" . translate p

        xOnly = translate . project (unit _x)
        yOnly = translate . project (unit _y)

        snapTo n p =
          addClass "smooth-drag" . translate (toGrid n <$> p)
        toGrid n x = fromInteger $ round x - (round x `mod` n)

        rotateByDistance p = rotate (Turn $ distance p 0 / 100)

        scaleByDistance p = scale (1 + distance p 0 / 250)

        shiftConfig = def
          { mouseEventFilter = \ e ->
              (button e == Auxiliary) && (Shift `elem` modifiers e ) }

        dragAnywhere = mdo
          (element, _) <- elDynAttr' "div" attributes $
            Dom.text "drag me!"
          let attributes = translate <$> total <*> pure [("class", "drag-me")]
          Drags { total } <- drags def element
          pure ()

        example :: Text
                -> DragConfig d t
                -> (Dynamic t (V2 Double) -> AttributeSet t "div" "HTML")
                -> m ()
        example description config doDrag = mdo
          label description
          (container, _) <- Html.div_ [ class_ =: ["draggable drag-example"] ] mdo
            (element, _) <- Html.div_ (doDrag total) (pure ())
            Drags { total } <- drags config { container = Just container } element
            pure ()
          pure ()

        snapBack = mdo
          label "Snap back after each drag"
          (container, _) <- Html.div_ [ class_ =: ["draggable drag-example"] ] mdo
            (element, _) <- Html.div_ (dragOrSnap current) (pure ())
            Drags { current, start, end } <- drags def { container = Just container } element

            -- TODO: some design that makes this behavior less
            -- convoluted?
            computed <- Reflex.performEvent $
              getComputedProperty element "transform" <$ start
            transform <- Reflex.holdDyn Nothing $
              Reflex.leftmost [computed, Nothing <$ end]
            let base = transform <&> \case
                  Just t  -> setProperty "transform" t []
                  Nothing -> []
            pure ()
          pure ()

        dragOrSnap = \case
          Just d  -> translate d
          Nothing -> transition snap
        snap = def { property = "transform", duration = s 1}

        dragHandle = mdo
          label "Drag entire diff by handle only."
          (container, _) <- elClass' "div" "drag-example" mdo
            (_, attributes) <- elDynAttr' "div" attributes mdo
              (handle, _) <- elDynAttr' "div" (pure [("class", "draggable drag-handle")]) do
                Dom.text "X"
              Drags { total, current } <- drags def {container = Just container } handle
              let isDragging = setClass "dragging" . isJust <$> current
              pure $ isDragging <*> (translate <$> total <*> pure [])
            pure ()
          pure ()

        enableDisable = mdo
          label "Enable or disable dragging interactively."
          (_, enabled) <- el' "div" do
            labelFor "toggle-drag" "Enabled dragging"
            snd <$> checkbox "toggle-drag" True Reflex.never
          (container, _) <- elClass' "div" "drag-example" mdo
            (element, _) <- elDynAttr' "div" attributes (pure ())
            let attributes = do
                  move   <- translate <$> total
                  class_ <- setClass "draggable" <$> enabled
                  pure $ class_ $ move []
                config = def
                  { container = Just container
                  , enabled   = Just $ enabledIf <$> enabled
                  }
            Drags { total } <- drags config element
            pure ()
          pure ()

        label :: Text -> m ()
        label = void . Html.div_ [ class_ =: ["label"] ] . Html.text

-- | Information about how a user interacts with an element by
-- dragging.
data Drags t = Drags
  { element  :: Html t
    -- ^ The element being dragged.

  , current  :: Dynamic t (Maybe (V2 Double))
    -- ^ The x and y distance moved by the mouse during the currently
    -- active drag. 'Nothing' when the user is not dragging this
    -- element.
    --
    -- If you just need to know wether an element is currently being
    -- dragged, you can use @isJust@:
    --
    -- @
    -- isDragged = isJust <$> current
    -- @

  , finished :: Dynamic t (V2 Double)
    -- ^ The total x and y distance moved by the mouse during /already
    -- finished/ drags. If the user is /currently/ dragging the
    -- element, the movement from that is not included.

  , total    :: Dynamic t (V2 Double)
    -- ^ The total x and y distance moved by the mouse counting /both/
    -- 'finished' and, if applicable, 'current'.

  , start    :: Event t (V2 Double)
    -- ^ The user started dragging the element at the given client X
    -- and Y coordinates.

  , end      :: Event t (V2 Double)
    -- ^ The user stopped dragging the element at the given client X
    -- and Y coordinates.
  }
  deriving stock (Generic)
             -- TODO: Does it make more sense for start and end to
             -- have the distance moved, just like the behaviors? If
             -- so, what distance: current or total?

-- | Configure how to measure drags for an item.
data DragConfig d t = DragConfig
  { container        :: Maybe (Html t)
  -- ^ Restrict the dragging to a container. Events outside the
  -- container do not count for the drag distance and don't end a
  -- drag.
  --
  -- The default is 'Nothing', which will use events from the entire
  -- document body.

  , enabled          :: Maybe (Dynamic t Bool)
  -- ^ Is dragging enabled for the element?
  --
  -- Default is 'Nothing', equivalent to "always enabled" (@pure
  -- Enabled@).
  --
  -- If 'enabled' becomes 'False' /while a drag is going on/, the drag
  -- will continue, but the user will not be able to drag the element
  -- again. (Note: this might change in the future when the 'drags'
  -- API is extended to make drags dynamically cancellable).

  -- enabled is a 'Maybe' because if we used 'Dynamic t Bool' and
  -- wrote the corresponding Default instance:
  --
  -- instance Reflex t => Default (DragConfig d t) where
  --   def = DragConfig { ..., enabled = pure True }
  --
  -- the type variable t would be ambiguous when overriding 'enabled':
  --
  -- def { enabled = ... }
  --
  -- using Maybe lets us write an instance without the Reflex t
  -- constraint, avoiding this problem
  --
  -- instance Default (DragConfig d t) where
  --   def = DragConfig { ..., enabled = Nothing }
  --
  -- this feels like a bit of a hack, but I have not been able to
  -- figure out a better solution

  , mouseEventFilter :: MouseEventResult -> Bool
  -- ^ A function to specify which mouse events can /start/ drags.
  --
  -- By default, this restricts to the main (usually left) mouse
  -- button:
  --
  -- @
  -- \ e -> button e == Main
  -- @
  --
  -- __Example__:
  --
  -- An interaction that only works when the user shift-clicks with
  -- the auxiliary (middle) mouse button:
  --
  -- @
  -- let mouseEventFilter e =
  --   (button e == Auxiliary) && (Shift `elem` modifiers e)
  -- in drags def { mouseEventFilter } draggableElement
  -- @
  }
  deriving stock (Generic)

instance Default (DragConfig d t) where
  def = DragConfig
    { container        = Nothing
    , enabled          = Nothing
    , mouseEventFilter = \ e -> button e == Main
    }

-- | Keeps track of how a user drags an element.
--
-- A "drag" starts when the user holds the main mouse button over the
-- element and ends when the user releases the mouse button.
--
-- The element itself does not have to move; this only provides the
-- /inputs/ from mouse drag actions. This gives you flexibility for
-- how to respond to the drag event: have the element follow the
-- cursor exactly, restrict to one dimension, snap to a 10px
-- grid... etc.
--
-- __Examples__
--
-- Set up a draggable element with a @doDrag@ that controls how the
-- element actuall drags:
--
-- @
-- draggable = do
--   rec let attributes = doDrag <$> total <*> pure []
--       (element, ()) <- elDynAttr' "div" attributes (pure ())
--       Drags { total } <- drags container element
--   pure ()
-- @
--
-- Follow the cursor exactly:
--
-- @
-- doDrag = translate
-- @
--
-- Restrict to x axis only:
--
-- @
-- doDrag = translate . view _x
-- @
--
-- Snap to a 10px grid:
--
-- @
-- doDrag = translate . fmap toGrid
--   where toGrid n x = fromInteger $ round x - round x `mod` n
-- @
--
-- Rotate based on the Euclidean distance the user drags:
--
-- @
-- doDrag p = rotate (Turn $ distance p 0 / 100)
-- @
drags :: forall m d t. Dom t m
      => DragConfig d t
      -- ^ Configuration for how to measure drags.
      -> Html t
      -- ^ The element that can be dragged.
      -> m (Drags t)
      -- ^ Two dynamics that combine to get the net move across /all/
      -- drags:
      --
      --  1. The net movement during the /current/ drag, 'Nothing' if
      --  the element is not being dragged.
      --
      --  2. The net movement from all /finished/ drags, /not/
      --  including the current drag.
drags DragConfig { container, enabled, mouseEventFilter } element = do
  -- TODO: Better error handling?
  body <- Document.getBodyUnsafe =<< Dom.askDocument
  (mouseup, mousemove) <- case container of
    Just e  ->
      pure (Dom.domEvent Dom.Mouseup e, Dom.domEvent Dom.Mousemove e)
    Nothing -> do
      up   <- performJs mouseEvent =<< body `on` "mouseup"
      move <- performJs mouseEvent =<< body `on` "mousemove"
      pure (up, move)
  rec let start = gate (canDrag <$> enabled' <*> isDragged) (client <$> mousedown)
          end   = gate isDragged (client <$> mouseup)
          move  = gate isDragged (client <$> mousemove)

          mousedown = Witherable.filter mouseEventFilter $
            Dom.domEvent Dom.Mousedown element

      isDragged <- Reflex.holdDyn False $ Reflex.leftmost [False <$ end, True <$ start]

      -- add .draggable class to body and container
      --
      -- we want to add to both so that we can turn off user-select
      -- across the entire document—otherwise, dragging outside the
      -- boundaries of the container highlights text in a jarring way
      Reflex.performEvent_ (dragging body <$ start)
      Reflex.performEvent_ (notDragging body <$ end)
      case container of
        Just container' -> do
          Reflex.performEvent_ (dragging container' <$ start)
          Reflex.performEvent_ (notDragging container' <$ end)
        Nothing -> pure ()

      -- current drag
      startPosition <- Reflex.holdDyn (V2 0 0) start
      let moves = Reflex.attachWith (flip (-)) (Reflex.current startPosition) move
          resets = V2 0 0 <$ end
      movedBy <- Reflex.holdDyn Nothing $ Just <$> Reflex.leftmost [resets, moves]
      let current = toMaybe <$> isDragged <*> movedBy

      -- completed drags
      let endDelta = Reflex.attachWith (flip (-)) (Reflex.current startPosition) end
      finished <- Reflex.foldDyn (+) (V2 0 0) endDelta

      let total = liftA2 (+) (fromMaybe 0 <$> current) finished

  pure Drags { element, current, finished, total, start, end }
  where toMaybe dragged delta = if dragged then delta else Nothing
        gate = Reflex.gate . Reflex.current

        canDrag dragEnabled isDragged = dragEnabled && not isDragged

        enabled' = fromMaybe (pure True) enabled

        dragging (rawElement -> e) = do
          existing <- fromMaybe ("" :: Text) <$>
            Element.getAttribute e ("class" :: Text)
          Element.setAttribute e ("class" :: Text) (existing <> " " <> "dragging")

        notDragging (rawElement -> e) = do
          let parseClasses = toList . classes . fromMaybe ""
              remove c = joinClasses . filter (/= c)
          existing <- parseClasses <$> Element.getAttribute e ("class" :: Text)
          Element.setAttribute e ("class" :: Text) $ remove "dragging" existing

main :: IO ()
main = withCss "css/ui-demo.css" (Runnable demo)
