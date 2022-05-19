{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Seven.Event where

import           Control.Lens             ((<&>))

import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import           Data.Word                (Word8)

import qualified GHCJS.DOM.ClipboardEvent as ClipboardEvent
import qualified GHCJS.DOM.DataTransfer   as DataTransfer
import qualified GHCJS.DOM.Element        as Element
import qualified GHCJS.DOM.EventM         as GHCJS
import qualified GHCJS.DOM.KeyboardEvent  as KeyboardEvent
import qualified GHCJS.DOM.MouseEvent     as MouseEvent

import qualified Reflex.Dom               as Dom

-- * Modifier Keys

-- | Modifier keys that can be held down during an event.
data Modifier = Ctrl | Shift | Alt | Meta
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- * Mouse Events

-- | Information from when the event triggered.
data MouseEventResult = MouseEventResult
  { screen    :: (Int, Int)
  -- ^ The cursor position in global (screen) coordinates.

  , client    :: (Int, Int)
  -- ^ The cursor position in local (DOM content) coordinates.

  , movement  :: (Int, Int)
  -- ^ The cursor position relative to the last 'Dom.Mousemove'
  -- event.

  , offset    :: (Int, Int)
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

  , button    :: Word
  -- ^ The number of the button that was pressed for the event.
  --
  -- TODO: Logically this should be a Maybe, but it isn't a 'Maybe' in
  -- @GHCJS.Dom.MouseEvent@, so who knows...
  }

-- | Build a 'MouseEvent' out of a raw JS mouse event.
getMouseEvent :: GHCJS.EventM e MouseEvent.MouseEvent MouseEventResult
getMouseEvent = do
  e <- GHCJS.event

  screen   <- (,) <$> MouseEvent.getScreenX e   <*> MouseEvent.getScreenY e
  client   <- (,) <$> MouseEvent.getClientX e   <*> MouseEvent.getClientY e
  movement <- (,) <$> MouseEvent.getMovementX e <*> MouseEvent.getMovementY e
  offset   <- (,) <$> MouseEvent.getOffsetX e   <*> MouseEvent.getOffsetY e

  ctrl  <- MouseEvent.getCtrlKey e
  shift <- MouseEvent.getShiftKey e
  alt   <- MouseEvent.getAltKey e
  meta  <- MouseEvent.getMetaKey e

  button <- MouseEvent.getButton e

  let modifiers = Set.fromList $
        [Ctrl | ctrl] <> [Shift | shift] <> [Alt | alt] <> [Meta | meta]

  pure $ MouseEventResult
    { screen, client, movement, offset, modifiers, button }

-- * Keyboard Events

-- | Information for a keyboard event containing things like keycode
-- and modifier keys.
data KeyboardEventResult = KeyboardEventResult
  { key       :: Key
  , modifiers :: Set Modifier
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

  pure $ KeyboardEventResult { modifiers, key }

-- * Clipboard Events

-- | Get the clipboard data that was pasted in as 'Text', if it's
-- available.
getPasteText :: GHCJS.EventM e ClipboardEvent.ClipboardEvent (Maybe Text)
getPasteText = do
  e <- GHCJS.event
  ClipboardEvent.getClipboardData e >>= \case
    Just dataTransfer -> Just <$> DataTransfer.getData dataTransfer ("text" :: Text)
    Nothing           -> pure Nothing

-- * Handling Events

type family EventResultType en where
  EventResultType 'Dom.ClickTag       = MouseEventResult
  EventResultType 'Dom.DblclickTag    = MouseEventResult
  EventResultType 'Dom.KeypressTag    = KeyboardEventResult
  EventResultType 'Dom.KeydownTag     = KeyboardEventResult
  EventResultType 'Dom.KeyupTag       = KeyboardEventResult
  EventResultType 'Dom.ScrollTag      = Double
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
  EventResultType 'Dom.MousewheelTag  = ()
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
  EventResultType 'Dom.WheelTag       = Dom.WheelEventResult

newtype EventResult en = EventResult { unEventResult :: EventResultType en }

domHandler :: Element.IsElement e
           => e
           -> Dom.EventName en
           -> GHCJS.EventM e (Dom.EventType en) (Maybe (EventResult en))
domHandler element eventName = Just . EventResult <$> case eventName of
  Dom.Click       -> getMouseEvent
  Dom.Dblclick    -> getMouseEvent
  Dom.Keypress    -> getKeyboardEvent
  Dom.Scroll      -> pure ()
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
  Dom.Paste       -> pure ()
  Dom.Reset       -> pure ()
  Dom.Search      -> pure ()
  Dom.Selectstart -> pure ()
  Dom.Touchstart  -> getTouchEvent
  Dom.Touchmove   -> getTouchEvent
  Dom.Touchend    -> getTouchEvent
  Dom.Touchcancel -> getTouchEvent
  Dom.Mousewheel  -> getMouseEvent
  Dom.Wheel       -> getWheelEvent
