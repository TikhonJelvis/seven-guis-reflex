{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Seven.Dialog where

import           Seven.Element
import           Seven.Event
import           Seven.Widget

import           Control.Lens                ((^.))
import           Control.Monad               (void)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.IO.Class      (liftIO)

import qualified Data.ByteString             as BS
import           Data.Map                    (Map)
import           Data.Text                   (Text)

import qualified GHCJS.DOM.Element           as GHCJS

import           Language.Javascript.JSaddle (JSVal, MakeObject, MonadJSM, fun,
                                              js, jsf, liftJSM)

import           Reflex
import qualified Reflex.Dom                  as Dom hiding (element)
import           Reflex.Dom                  hiding (EventResult,
                                              EventResultType, button,
                                              elDynAttr', element)

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
  { element :: Element EventResult d t
  , close   :: Event t ()
  , cancel  :: Event t ()
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

  cancel <- onCancel element
  close  <- onClose element

  pure (DialogElement { element, cancel, close }, result)
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
setDialogState :: (MonadJSM m, MakeObject (RawElement d))
               => Element er d t
               -> ModalState
               -> m ()
setDialogState dialog = void . liftJSM . \case
  Show      -> raw ^. jsf ("show" :: Text) ()
  ShowModal -> raw ^. jsf ("showModal" :: Text) ()
  Hide      -> raw ^. jsf ("hide" :: Text) ()
  where raw = _element_raw dialog

-- | An 'Event' that triggers when the given @dialog@ element is
-- closed.
--
-- See MDN: [@close@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
onClose :: (TriggerEvent t m, MakeObject (RawElement d), MonadJSM m)
        => Element er d t
        -> m (Event t ())
onClose dialog = do
  (event, trigger) <- newTriggerEvent
  let jsTrigger _f _this _args = liftIO $ trigger ()
  liftJSM $ raw ^. jsf ("addEventListener" :: Text) ("close" :: Text, fun jsTrigger)
  pure event
  where raw = _element_raw dialog

-- | An 'Event' that triggers when the given @dialog@ element is
-- closed.
--
-- See MDN: [@cancel@](https://developer.mozilla.org/en-US/docs/Web/API/HTMLDialogElement/close_event)
onCancel :: (TriggerEvent t m, MakeObject (RawElement d), MonadJSM m)
         => Element er d t
         -> m (Event t ())
onCancel dialog = do
  (event, trigger) <- newTriggerEvent
  let jsTrigger _f _this _args = liftIO $ trigger ()
  liftJSM $ raw ^. jsf ("addEventListener" :: Text) ("cancel" :: Text, fun jsTrigger)
  pure event
  where raw = _element_raw dialog

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css do
    press <- Dom.button "Hello"
    DialogElement { close, cancel } <- alert ("Hello, World!" <$ press)
    countClose <- count close
    countCancel <- count cancel
    output @Int countClose
    output @Int countCancel
    pure ()
