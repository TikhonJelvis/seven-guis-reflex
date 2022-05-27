{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module UI.Dialog where

import           Control.Lens                ((^.))
import           Control.Monad               (unless, void, when)

import qualified Data.ByteString             as BS
import           Data.Map                    (Map)
import           Data.Text                   (Text)

import           Language.Javascript.JSaddle (MakeObject, MonadJSM, jsf,
                                              liftJSM, valToBool, (!))

import           Reflex
import qualified Reflex.Dom                  as Dom hiding (element)
import           Reflex.Dom                  hiding (EventResult,
                                              EventResultType, button,
                                              elDynAttr', element)

import           UI.Element
import           UI.Event
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
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | The 'Element' for a dialog along with events for when the dialog
-- is canceled or closed.
--
-- Related MDN documentation:
--
--   * [@HTMLDialogElement@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement)
--   * [@cancel@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/cancel_event)
--   * [@close@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
data DialogElement d t = DialogElement
  { element  :: Element EventResult d t
  , closed   :: Event t ()
  , canceled :: Event t ()
  }

-- | Create a floating dialog using the HTML5 @dialog@ element.
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
--   void $ dialog (Show <$ press) (constDyn []) (text "Hello, World!")
-- @
dialog :: forall a m t. (Dom t m)
       => Event t ModalState
       -- ^ Events controlling when the dialog is shown or closed.
       -> Dynamic t (Map Text Text)
       -- ^ Dynamic attributes
       -> m a
       -- ^ The dialog body
       -> m (DialogElement (DomBuilderSpace m) t, a)
dialog states attrs body = do
  (element, result) <- elDynAttr' "dialog" attrs body
  performEvent_ $ setDialogState element <$> states

  canceled <- onCancel element
  closed   <- onClose element

  pure (DialogElement { element, closed, canceled }, result)
{-# INLINABLE dialog #-}

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
      -> m (DialogElement (DomBuilderSpace m) t)
alert trigger = do
  (element, _) <- dialog (ShowModal <$ trigger) attrs do
    message <- holdDyn "" trigger
    dynText message
    elAttr "form" [("method", "dialog")] $ Dom.button "Ok"
  pure element
  where attrs = constDyn [("class", "alert")]

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
setDialogState :: (MonadJSM m, MakeObject (RawElement d))
               => Element er d t
               -> ModalState
               -> m ()
setDialogState element state = liftJSM do
  open <- valToBool =<< raw ! ("open" :: Text)
  case state of
    Show      -> unless open $ void $ raw ^. jsf ("show" :: Text) ()
    ShowModal -> unless open $ void $ raw ^. jsf ("showModal" :: Text) ()
    Hide      -> when open   $ void $ raw ^. jsf ("hide" :: Text) ()
  where raw = _element_raw element

-- | An 'Event' that triggers when the given @dialog@ element is
-- closed.
--
-- See MDN: [@close@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
onClose :: (TriggerEvent t m, MakeObject (RawElement d), MonadJSM m, Reflex t)
        => Element er d t
        -> m (Event t ())
onClose element = void <$> element `on` "close"

-- | An 'Event' that triggers when the given @dialog@ element is
-- closed.
--
-- See MDN: [@cancel@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
onCancel :: (TriggerEvent t m, MakeObject (RawElement d), MonadJSM m, Reflex t)
         => Element er d t
         -> m (Event t ())
onCancel element = void <$> element `on` "cancel"


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css do
    press <- Dom.button "Hello"
    DialogElement { closed, canceled } <- alert ("Hello, World!" <$ press)
    countClose <- count closed
    countCancel <- count canceled
    output @Int countClose
    output @Int countCancel
    pure ()
