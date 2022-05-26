{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module UI.Event where

import           Control.Lens                (element, (<&>), (^.))
import           Control.Monad.IO.Class      (MonadIO (liftIO))

import           Data.Functor.Misc           (WrapArg (..))
import           Data.Hashable               (Hashable)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Data.Word                   (Word8)

import           GHC.Generics                (Generic)

import qualified GHCJS.DOM.ClipboardEvent    as ClipboardEvent
import qualified GHCJS.DOM.DataTransfer      as DataTransfer
import qualified GHCJS.DOM.Element           as Element
import qualified GHCJS.DOM.Event             as Event
import qualified GHCJS.DOM.EventM            as GHCJS
import qualified GHCJS.DOM.KeyboardEvent     as KeyboardEvent
import qualified GHCJS.DOM.MouseEvent        as MouseEvent
import qualified GHCJS.DOM.Types             as GHCJS
import qualified GHCJS.DOM.UIEvent           as UIEvent
import qualified GHCJS.DOM.WheelEvent        as WheelEvent
import qualified GHCJS.Types                 as GHCJS

import           Language.Javascript.JSaddle (liftJSM, showJSValue, valToNumber,
                                              (!))
import qualified Language.Javascript.JSaddle as Js

import qualified Reflex
import qualified Reflex.Dom                  as Dom
import           Reflex.Dom                  (HasDomEvent, Reflex)

import           Text.Printf                 (printf)

import           UI.Point

-- * Types of Events

type family EventResultType en where
  EventResultType 'Dom.ClickTag       = MouseEventResult
  EventResultType 'Dom.DblclickTag    = MouseEventResult
  EventResultType 'Dom.KeypressTag    = KeyboardEventResult
  EventResultType 'Dom.KeydownTag     = KeyboardEventResult
  EventResultType 'Dom.KeyupTag       = KeyboardEventResult
  EventResultType 'Dom.ScrollTag      = ScrollEventResult
  EventResultType 'Dom.MousemoveTag   = MouseEventResult
  EventResultType 'Dom.MousedownTag   = MouseEventResult
  EventResultType 'Dom.MouseupTag     = MouseEventResult
  EventResultType 'Dom.MouseenterTag  = MouseEventResult
  EventResultType 'Dom.MouseleaveTag  = MouseEventResult
  EventResultType 'Dom.FocusTag       = ()
  EventResultType 'Dom.BlurTag        = ()
  EventResultType 'Dom.ChangeTag      = ()
  EventResultType 'Dom.DragTag        = MouseEventResult
  EventResultType 'Dom.DragendTag     = MouseEventResult
  EventResultType 'Dom.DragenterTag   = MouseEventResult
  EventResultType 'Dom.DragleaveTag   = MouseEventResult
  EventResultType 'Dom.DragoverTag    = MouseEventResult
  EventResultType 'Dom.DragstartTag   = MouseEventResult
  EventResultType 'Dom.DropTag        = MouseEventResult
  EventResultType 'Dom.AbortTag       = ()
  EventResultType 'Dom.ContextmenuTag = MouseEventResult
  EventResultType 'Dom.ErrorTag       = ()
  EventResultType 'Dom.InputTag       = ()
  EventResultType 'Dom.InvalidTag     = ()
  EventResultType 'Dom.LoadTag        = ()
  EventResultType 'Dom.MouseoutTag    = MouseEventResult
  EventResultType 'Dom.MouseoverTag   = MouseEventResult
  EventResultType 'Dom.MousewheelTag  = MouseEventResult
  EventResultType 'Dom.SelectTag      = ()
  EventResultType 'Dom.SubmitTag      = ()
  EventResultType 'Dom.BeforecutTag   = ()
  EventResultType 'Dom.CutTag         = ()
  EventResultType 'Dom.BeforecopyTag  = ()
  EventResultType 'Dom.CopyTag        = ()
  EventResultType 'Dom.BeforepasteTag = ()
  EventResultType 'Dom.PasteTag       = Maybe Text
  EventResultType 'Dom.ResetTag       = ()
  EventResultType 'Dom.SearchTag      = ()
  EventResultType 'Dom.SelectstartTag = ()
  EventResultType 'Dom.TouchstartTag  = Dom.TouchEventResult
  EventResultType 'Dom.TouchmoveTag   = Dom.TouchEventResult
  EventResultType 'Dom.TouchendTag    = Dom.TouchEventResult
  EventResultType 'Dom.TouchcancelTag = Dom.TouchEventResult
  EventResultType 'Dom.WheelTag       = WheelEventResult

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

domHandler :: Dom.EventName en
           -> GHCJS.EventM e (Dom.EventType en) (Maybe (EventResult en))
domHandler eventName = Just . EventResult <$> case eventName of
  Dom.Click       -> getMouseEvent
  Dom.Dblclick    -> getMouseEvent
  Dom.Keypress    -> getKeyboardEvent
  Dom.Scroll      -> getScrollEvent
  Dom.Keydown     -> getKeyboardEvent
  Dom.Keyup       -> getKeyboardEvent
  Dom.Mousemove   -> getMouseEvent
  Dom.Mouseup     -> getMouseEvent
  Dom.Mousedown   -> getMouseEvent
  Dom.Mouseenter  -> getMouseEvent
  Dom.Mouseleave  -> getMouseEvent
  Dom.Focus       -> pure ()
  Dom.Blur        -> pure ()
  Dom.Change      -> pure ()
  Dom.Drag        -> getMouseEvent
  Dom.Dragend     -> getMouseEvent
  Dom.Dragenter   -> getMouseEvent
  Dom.Dragleave   -> getMouseEvent
  Dom.Dragover    -> getMouseEvent
  Dom.Dragstart   -> getMouseEvent
  Dom.Drop        -> getMouseEvent
  Dom.Abort       -> pure ()
  Dom.Contextmenu -> getMouseEvent
  Dom.Error       -> pure ()
  Dom.Input       -> pure ()
  Dom.Invalid     -> pure ()
  Dom.Load        -> pure ()
  Dom.Mouseout    -> getMouseEvent
  Dom.Mouseover   -> getMouseEvent
  Dom.Select      -> pure ()
  Dom.Submit      -> pure ()
  Dom.Beforecut   -> pure ()
  Dom.Cut         -> pure ()
  Dom.Beforecopy  -> pure ()
  Dom.Copy        -> pure ()
  Dom.Beforepaste -> pure ()
  Dom.Paste       -> getPasteText
  Dom.Reset       -> pure ()
  Dom.Search      -> pure ()
  Dom.Selectstart -> pure ()
  Dom.Touchstart  -> Dom.getTouchEvent
  Dom.Touchmove   -> Dom.getTouchEvent
  Dom.Touchend    -> Dom.getTouchEvent
  Dom.Touchcancel -> Dom.getTouchEvent
  Dom.Mousewheel  -> getMouseEvent
  Dom.Wheel       -> getWheelEvent

instance Reflex t => HasDomEvent t (Dom.Element EventResult d t) en where
  type DomEventType (Dom.Element EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en e =
    Dom.coerceEvent $ Reflex.select (Dom._element_events e) (WrapArg en)

-- * Modifier Keys

-- | Modifier keys that can be held down during an event.
data Modifier = Ctrl | Shift | Alt | Meta
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- * Mouse Events

-- | Specifies which mouse button was clicked to trigger an event.
--
-- Note that a mouse may be configured different from the standard
-- left-right button; for example, left-handed users often reverse
-- left- and right-clicking.
--
-- See MDN: [@MouseEvent.button@](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button)
data MouseButton = Main
                 -- ^ The main, usually left, button.
                 | Auxiliary
                 -- ^ A third, extra button, usually the middle button
                 -- or mouse wheel.
                 | Secondary
                 -- ^ The secondary, usually right button.
                 | Fourth
                 -- ^ The fourth button, often a browser back button.
                 | Fifth
                 -- ^ The fifth button, often a browser forward
                 -- button.
                 | UnknownButton !Word
                 -- ^ Some other, unknown mouse button.
  deriving stock (Show, Eq)

-- TODO: Separate out "click"-style events (mousedown, mouseup, click)
-- from "move"-style events (mouseenter, mouseleave, mouseover,
-- mouseout, mousemove) since the 'button' field for the latter does
-- not make sense.

-- | Information from when the event triggered.
data MouseEventResult = MouseEventResult
  { screen    :: !Point
  -- ^ The cursor position in global (screen) coordinates.

  , client    :: !Point
  -- ^ The cursor position in local (DOM content) coordinates.

  , movement  :: !Point
  -- ^ The cursor position relative to the last 'Dom.Mousemove'
  -- event.
  --
  -- Warning: this currently does not seem to work correctly on
  -- WebkitGtk, instead providing the same numbers as 'client'.

  , offset    :: !Point
  -- ^ The cursor position relative to the element of the event's
  -- target node. The relative position is calculated relative to the
  -- position of the padding edge of the target node.
  --
  -- Note: the offset position is calculated relative to the target
  -- element, which is not necessarily the element that had the event
  -- listener unchecked. This means that 'offset' may be inconsistent
  -- when a user clicks on nested or overlapping elements.

  , modifiers :: Set Modifier
  -- ^ Any modifier keys that were held down when the event triggered.

  , button    :: !MouseButton
  -- ^ The number of the button that was pressed for the event.
  --
  -- TODO: Logically this should be a Maybe, but it isn't a 'Maybe' in
  -- @GHCJS.Dom.MouseEvent@, so who knows...
  }

-- | Build a 'MouseEvent' out of a raw JS mouse event.
getMouseEvent :: GHCJS.EventM e MouseEvent.MouseEvent MouseEventResult
getMouseEvent = do
  e <- GHCJS.event

  val <- GHCJS.liftJSM $ GHCJS.toJSVal e

  screen   <- point <$> MouseEvent.getScreenX e   <*> MouseEvent.getScreenY e
  client   <- point <$> MouseEvent.getClientX e   <*> MouseEvent.getClientY e
  movement <- point <$> MouseEvent.getMovementX e <*> MouseEvent.getMovementY e
  offset   <- point <$> MouseEvent.getOffsetX e   <*> MouseEvent.getOffsetY e

  ctrl  <- MouseEvent.getCtrlKey e
  shift <- MouseEvent.getShiftKey e
  alt   <- MouseEvent.getAltKey e
  meta  <- MouseEvent.getMetaKey e

  button <- MouseEvent.getButton e <&> \case
    0 -> Main
    1 -> Auxiliary
    2 -> Secondary
    3 -> Fourth
    4 -> Fifth
    x -> UnknownButton x

  let modifiers = Set.fromList $
        [Ctrl | ctrl] <> [Shift | shift] <> [Alt | alt] <> [Meta | meta]

  pure MouseEventResult
    { screen, client, movement, offset, modifiers, button }

-- ** Wheel Events

-- | Information about how the mouse wheel moved to trigger a
-- wheel event.
data WheelEventResult = WheelEventResult
  { delta     :: (Double, Double, Double)
    -- ^ The x, y and z deltas respectively. These specify how the
    -- wheel moved along three axes.
  , deltaMode :: !DeltaMode
    -- ^ How the deltas were measured.
  }

-- | How the change in position for a wheel event was measured.
data DeltaMode = Delta_Line
               | Delta_Pixel
               | Delta_Page
               | Delta_Unknown Word
  deriving stock (Show, Eq, Ord)


-- TODO: Get MouseEvent properties for WheelEvent too?
-- | Build a 'WheelEventResult' from a JS event.
getWheelEvent :: GHCJS.EventM e WheelEvent.WheelEvent WheelEventResult
getWheelEvent = do
  e <- GHCJS.event

  x <- WheelEvent.getDeltaX e
  y <- WheelEvent.getDeltaY e
  z <- WheelEvent.getDeltaZ e

  deltaMode <- WheelEvent.getDeltaMode e <&> \case
    0 -> Delta_Line
    1 -> Delta_Pixel
    2 -> Delta_Page
    x -> Delta_Unknown x

  pure WheelEventResult { delta = (x, y, z), deltaMode }

-- ** Scrolling

-- | The information accompanying a scroll event.
data ScrollEventResult = ScrollEventResult
  { scrollTop  :: !Double
  -- ^ How much the target element is scrolled vertically.
  , scrollLeft :: !Double
  -- ^ How much the target element is scrolled horizontally.
  --
  -- If the element's @direction@ is @rtl@, this is calculated from
  -- the right instead.
  }
  deriving stock (Show, Eq, Ord)

-- | Build a 'ScrollEventResult' from a scroll event.
--
-- This works fine for any 'UIEvent.UIEvent', so it's only
-- scroll-specific because it returns information on how the target
-- element is scrolled.
getScrollEvent :: GHCJS.EventM e UIEvent.UIEvent ScrollEventResult
getScrollEvent = do
  e <- GHCJS.event
  element <- GHCJS.uncheckedCastTo Element.Element <$> Event.getTargetUnchecked e

  scrollLeft <- fromIntegral <$> Element.getScrollLeft element
  scrollTop  <- fromIntegral <$> Element.getScrollTop element

  pure ScrollEventResult { scrollLeft, scrollTop }

-- * Keyboard Events

-- | Information for a keyboard event containing things like keycode
-- and modifier keys.
data KeyboardEventResult = KeyboardEventResult
  { key       :: !Key
  , modifiers :: !(Set Modifier)
  }

-- TODO: Replace with a structure type—straightforward, but it was
-- just a bit too tedious to write this up before getting other things
-- working...

-- | The key pressed to trigger a keyboard event.
--
-- For keys that correspond to printable Unicode characters, this will
-- contain exactly those characters
--
-- Other keys have special strings like @"Control"@ or @"Fn"@; see the
-- MDN documentation for a full list:
-- <https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values>
newtype Key = Key Text
  deriving stock (Show, Eq, Ord)

-- | The key location associated with the key that triggered the
-- keyboard event. This lets you distinguish paired keys like left
-- shift vs right shift as well as distinguishing the numpad from the
-- normal keyboard.
data KeyLocation = KeyLocation_Standard
                 -- ^ The key is in a "standard" location (ie not on
                 -- the numpad). For keys like @Ctrl@ that normally
                 -- have left and right variants, this means the key
                 -- was not associated with either.

                 | KeyLocation_Left
                 -- ^ Some keys like @Shift@ and @Ctrl@ have two
                 -- locations on a keyboard. This identifies the left
                 -- instance of those keys.

                 | KeyLocation_Right
                 -- ^ Some keys like @Shift@ and @Ctrl@ have two
                 -- locations on a keyboard. This identifies the right
                 -- instance of those keys.

                 | KeyLocation_Numpad
                 -- ^ The key is located on a separate numpad. This
                 -- does not include the @NumLock@ key which is always
                 -- 'Standard'.

                 | KeyLocation_Unknown Word
                 -- ^ A location code that is not standard.

-- | Build a 'KeyboardEventResult' from the native JS event object.
getKeyboardEvent :: GHCJS.EventM e KeyboardEvent.KeyboardEvent KeyboardEventResult
getKeyboardEvent = do
  e <- GHCJS.event

  key <- Key <$> KeyboardEvent.getKey e

  location <- KeyboardEvent.getLocation e <&> \case
    0 -> KeyLocation_Standard
    1 -> KeyLocation_Left
    2 -> KeyLocation_Right
    3 -> KeyLocation_Numpad
    x -> KeyLocation_Unknown x

  ctrl  <- KeyboardEvent.getCtrlKey e
  shift <- KeyboardEvent.getShiftKey e
  alt   <- KeyboardEvent.getAltKey e
  meta  <- KeyboardEvent.getMetaKey e

  let modifiers = Set.fromList $
        [Ctrl | ctrl] <> [Shift | shift] <> [Alt | alt] <> [Meta | meta]

  pure KeyboardEventResult { modifiers, key }

-- * Clipboard Events

    -- TODO: API for other kinds of copy/paste data?

-- | Get the clipboard data that was pasted in as 'Text', if it's
-- available.
getPasteText :: GHCJS.EventM e ClipboardEvent.ClipboardEvent (Maybe Text)
getPasteText = do
  e <- GHCJS.event
  ClipboardEvent.getClipboardData e >>= \case
    Just dataTransfer -> Just <$> DataTransfer.getData dataTransfer ("text" :: Text)
    Nothing           -> pure Nothing

-- * Wrapping JS Events

-- | Return an 'Event' that triggers based on an arbitrary JavaScript
-- event on the given element.
-- Conceptually, this is equvivalent to:
--
-- @
-- element.addEventListener("eventName", function (e) {
--   trigger(e)
-- }
-- @
--
-- For example, if you wanted to listen to the (IE-only)
-- @MSGestureStart@, you could write:
--
-- @
-- data MSGestureEvent = MSGestureEvent {- ... -}
--
-- toMSGestureEvent :: JSVal -> JSM MSGestureEvent
-- toMSGestureEvent = {- ... -}
--
-- onGestureStart :: (TriggerEvent t m, MakeObject (RawElement d), MonadJSM m, Reflex t)
--                => Element er d t
--                -> m (Event t MSGestureEvent)
-- onGestureStart element =
--   addEventListener element "MSGestureChange" toMSGestureEvent
-- @
--
-- If you don't care about the value at each event, you can use @\ _
-- -> pure ()@ to ignore it:
--
-- @
-- events :: _ => m (Event t ())
-- events = addEventListener element "onsomeevent" (\ _ -> pure ())
-- @
--
-- See MDN: [@addEventListener@](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener)
addEventListener :: ( Reflex.TriggerEvent t m
                    , Js.MakeObject (Dom.RawElement d)
                    , Js.MonadJSM m
                    )
                 => Dom.Element er d t
                 -> Text
                 -> (Js.JSVal -> Js.JSM a)
                 -- ^ Function to transform the type of the object at
                 -- each event.
                 -> m (Reflex.Event t a)
addEventListener element eventName f = do
  (event, trigger) <- Reflex.newTriggerEvent
  let jsTrigger _f _this = \case
        [e] -> do
          e' <- f e
          liftIO $ trigger e'
        args ->
          error $ printf "Event listener callback called with %d ≠ 1 args" (length args)

  Js.liftJSM $ raw ^. Js.jsf ("addEventListener" :: Text) (eventName, Js.fun jsTrigger)
  pure event
  where raw = Dom._element_raw element
