{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
module Seven.Event where

import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)

import qualified GHCJS.DOM.Element    as Element
import qualified GHCJS.DOM.EventM     as GHCJS
import qualified GHCJS.DOM.MouseEvent as MouseEvent

import qualified Reflex.Dom           as Dom

-- * Modifier Keys

-- | Modifier keys that can be held down during an event.
data Modifier = Ctrl | Shift | Alt | Meta
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- * Mouse Events

-- | Information attached to a mouse event: modifiers, movement and
-- mouse position.
data MouseEvent = MouseEvent
  { screen    :: (Int, Int)
  , client    :: (Int, Int)
  , movement  :: (Int, Int)
  , offset    :: (Int, Int)
  , modifiers :: Set Modifier
  }

-- | Build a 'MouseEvent' out of a raw JS mouse event.
getMouseEvent :: GHCJS.EventM e MouseEvent.MouseEvent MouseEvent
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

  let modifiers = Set.fromList $
        [Ctrl | ctrl] <> [Shift | shift] <> [Alt | alt] <> [Meta | meta]

  pure $ MouseEvent { screen, client, movement, offset, modifiers }

-- * Handling Events

type family EventResultType en where
  EventResultType 'Dom.ClickTag       = MouseEvent
  EventResultType 'Dom.DblclickTag    = MouseEvent
  EventResultType 'Dom.KeypressTag    = Word
  EventResultType 'Dom.KeydownTag     = Word
  EventResultType 'Dom.KeyupTag       = Word
  EventResultType 'Dom.ScrollTag      = Double
  EventResultType 'Dom.MousemoveTag   = MouseEvent
  EventResultType 'Dom.MousedownTag   = MouseEvent
  EventResultType 'Dom.MouseupTag     = MouseEvent
  EventResultType 'Dom.MouseenterTag  = MouseEvent
  EventResultType 'Dom.MouseleaveTag  = MouseEvent
  EventResultType 'Dom.FocusTag       = ()
  EventResultType 'Dom.BlurTag        = ()
  EventResultType 'Dom.ChangeTag      = ()
  EventResultType 'Dom.DragTag        = MouseEvent
  EventResultType 'Dom.DragendTag     = MouseEvent
  EventResultType 'Dom.DragenterTag   = MouseEvent
  EventResultType 'Dom.DragleaveTag   = MouseEvent
  EventResultType 'Dom.DragoverTag    = MouseEvent
  EventResultType 'Dom.DragstartTag   = MouseEvent
  EventResultType 'Dom.DropTag        = ()
  EventResultType 'Dom.AbortTag       = ()
  EventResultType 'Dom.ContextmenuTag = MouseEvent
  EventResultType 'Dom.ErrorTag       = ()
  EventResultType 'Dom.InputTag       = ()
  EventResultType 'Dom.InvalidTag     = ()
  EventResultType 'Dom.LoadTag        = ()
  EventResultType 'Dom.MouseoutTag    = MouseEvent
  EventResultType 'Dom.MouseoverTag   = MouseEvent
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
domHandler element eventName = undefined
