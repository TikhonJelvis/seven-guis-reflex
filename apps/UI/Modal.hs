module UI.Modal
  ( modal
  , alert

  , ModalState (..)
  , ModalElement (..)

  , setDialogState
  )
where

import           Control.Lens                      ((^.))
import           Control.Monad                     (unless, void, when)

import           Data.Hashable                     (Hashable)
import           Data.Text                         (Text)

import           GHC.Generics                      (Generic)

import           Language.Javascript.JSaddle       (MonadJSM, jsf, liftJSM,
                                                    valToBool, (!))

import qualified Reflex
import           Reflex                            (Event)
import qualified Reflex.Dom                        as Dom

import           UI.Attributes                     (class_)
import           UI.Attributes.AttributeSet.Reflex (AttributeSet, (=:))
import           UI.Element
import           UI.Element.IsElement              (IsElement (..))
import           UI.Event                          (on)
import           UI.Html                           (Html, html)
import           UI.Main                           (Runnable (..), withCss)
import           UI.Widget

-- | HTML modal dialogs can either be hidden or shown in two ways:
--
--   * 'ShowModal' displays the dialog on top of any other content in
--     the page and disables interacting with other page elements. The
--     dialog is displayed in front of the @::backdrop@ pseudoelement,
--     letting you dim the rest of the page.
--
--   * 'Show' displays the dialog but lets users interact with the
--     rest of the page and does /not/ display a @::backdrop@
--     pseudoelement.
--
--   * 'Hide' closes the dialog.
--
-- See the MDN documentation for more details:
--
--  * [@showModal@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/showModal)
--  * [@show@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/show)
--  * [@cancel@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/cancel)
data ModalState = Show | ShowModal | Hide
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | The 'Element' for a modal dialog along with events for when the
-- modal is canceled or closed.
--
-- Related MDN documentation:
--
--   * [@HTMLDialogElement@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement)
--   * [@cancel@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/cancel_event)
--   * [@close@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
data ModalElement t = ModalElement
  { element  :: Html t
  -- ^ The @dialog@ HTML element itself.

  , closed   :: Event t ()
  -- ^ An 'Event' that triggers when the given @dialog@ element is
  -- closed.
  --
  -- See MDN:
  -- [@close@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)

  , canceled :: Event t ()
  -- ^ An 'Event' that triggers when the given @dialog@ element is
  -- canceled.
  --
  -- See MDN:
  -- [@cancel@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
  }
  deriving stock (Generic)

instance IsElement (ModalElement t) where rawElement = rawElement . element

-- | Create a modal dialog using the HTML5 @dialog@ element.
--
-- The dialog will be added to the page but hidden until a 'Show' or
-- 'ShowModal' event fires.
--
-- Here's an example of popping up a @Hello, World!@ dialog on a
-- button press:
--
-- @
-- mainWidget do
--   press <- button "Hello"
--   void $ dialog (Show <$ press) (pure []) (text "Hello, World!")
-- @
modal :: forall a m t. (Dom t m)
      => Event t ModalState
      -- ^ Events controlling when the dialog is shown or closed.
      -> AttributeSet t "dialog" "HTML"
      -- ^ attributes
      -> m a
      -- ^ dialog body
      -> m (ModalElement t, a)
modal states attributes body = do
  (element, result) <- html attributes body
  Reflex.performEvent_ $ setDialogState element <$> states

  canceled <- element `on` "cancel"
  closed   <- element `on` "close"

  pure (ModalElement { element, closed, canceled }, result)
{-# INLINABLE modal #-}

-- | When the input 'Event' triggers, show the user a modal dialog
-- window with the message from the 'Event' and an "Ok" button. The
-- dialog will only close when dismissed by the user.
--
-- The @dialog@ element will have the @alert@ CSS class.
--
-- Note: this uses a modal HTML @dialog@ element, not a browser-native
-- alert.
--
-- Alerting on a button press:
--
-- @
-- mainWidget do
--   press <- button "Hello"
--   void $ alert ("Hello, World!" <$ press)
-- @
alert :: forall m t. (Dom t m)
      => Event t Text
      -> m (ModalElement t)
alert trigger = do
  (element, _) <- modal (ShowModal <$ trigger) [ class_ =: "alert" ] do
    message <- Reflex.holdDyn "" trigger
    Dom.dynText message
    Dom.elAttr "form" [("method", "dialog")] $ Dom.button "Ok"
  pure element

-- ** JS API

-- $ Thin wrappers over JavaScript functions to interact with raw
-- @dialog@ element objects.

-- | Explicitly set the dialog state.
--
-- If the dialog is already open, 'Show' and 'ShowModal' do nothing.
--
-- If the dialog is already closed, 'Hide' does nothing.
--
-- Note that the dialog can be closed through user actions outside of
-- Haskell code, so it /is/ possible for two 'Show'/'ShowModal' events
-- in a row to both have an effect.
setDialogState :: (MonadJSM m, IsElement e)
               => e
               -> ModalState
               -> m ()
setDialogState (rawElement -> raw) state = liftJSM do
  open <- valToBool =<< raw ! ("open" :: Text)
  case state of
    Show      -> unless open $ void $ raw ^. jsf ("show" :: Text) ()
    ShowModal -> unless open $ void $ raw ^. jsf ("showModal" :: Text) ()
    Hide      -> when open   $ void $ raw ^. jsf ("hide" :: Text) ()

_demo :: IO ()
_demo = do
  withCss "css/ui.css" $ Runnable do
    press <- Dom.button "Hello"
    ModalElement { closed, canceled } <- alert ("Hello, World!" <$ press)
    countClose <- Reflex.count closed
    countCancel <- Reflex.count canceled
    output @Int countClose
    output @Int countCancel
    pure ()
