{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Seven.Element where

import           Seven.Event

import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (runReaderT)

import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)

import           Language.Javascript.JSaddle (MonadJSM)

import           Reflex
import qualified Reflex.Dom                  as Dom
import           Reflex.Dom                  (DomBuilder (..),
                                              DomSpace (EventSpec),
                                              ElementConfig (..))


type Dom t m = ( MonadFix m
               , MonadHold t m
               , PerformEvent t m
               , MonadJSM (Performable m)
               , TriggerEvent t m
               , PostBuild t m
               , DomBuilder t m
               , DomBuilderSpace m ~ Dom.GhcjsDomSpace
               , MonadJSM m
               )

-- * Elements

el' :: forall a m t. Dom t m
    => Text
    -> m a
    -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
el' tag = elAttr' tag []

-- | Create and return an element with the given class.
elClass' :: forall a m t. Dom t m
         => Text
         -> Text
         -> m a
         -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
elClass' tag class_ = elAttr' tag [("class", class_)]
{-# INLINABLE elClass' #-}

-- | Create and return an element with the given attributes.
elAttr' :: forall a m t. Dom t m
        => Text
        -> Map Text Text
        -> m a
        -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
elAttr' tag attr = elDynAttr' tag (constDyn attr)
{-# INLINABLE elAttr' #-}

-- | Create and return an elment with a dynamically changing set of
-- options.
elDynAttr' :: forall a m t. Dom t m
           => Text
           -> Dynamic t (Map Text Text)
           -> m a
           -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
elDynAttr' = elDynAttrNs' Nothing
{-# INLINABLE elDynAttr' #-}

-- | Create and return an element in the given (optional) namespace.
elDynAttrNs' :: forall a m t. Dom t m
             => Maybe Text
             -- ^ Optional namespace
             -> Text
             -- ^ Tag
             -> Dynamic t (Map Text Text)
             -- ^ Dynamic attributes
             -> m a
             -- ^ Body
             -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
elDynAttrNs' namespace tag attrs body = do
  modifyAttrs <- Dom.dynamicAttributesToModifyAttributes attrs
  let config = ElementConfig
        { _elementConfig_namespace = namespace
        , _elementConfig_initialAttributes = Map.empty
        , _elementConfig_modifyAttributes =
          Just $ fmapCheap Dom.mapKeysToAttributeName modifyAttrs
        , _elementConfig_eventSpec = eventSpec
        }
  result <- element tag config body
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result
  where eventSpec = Dom.GhcjsEventSpec filters handler
        filters = mempty
        handler = Dom.GhcjsEventHandler \ (eventName, event) ->
          runReaderT (domHandler eventName) (Dom.unGhcjsDomEvent event)
{-# INLINABLE elDynAttrNs' #-}
