{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module UI.Element
  ( PerformJS
  , Dom

  , IsElement
  , Element

  , el'
  , elClass'
  , elAttr'
  , elDynAttr'
  , elDynAttrNs'

  , Rectangle
  , bounds
  , dimensions
  , viewportPosition
  , offsetPosition
  )
where

import           Control.Monad.Fix           (MonadFix)

import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)

import qualified GHCJS.DOM.DOMRect           as GHCJS
import qualified GHCJS.DOM.Element           as GHCJS
import qualified GHCJS.DOM.HTMLElement       as GHCJS
import qualified GHCJS.DOM.Node              as GHCJS
import           GHCJS.DOM.Types             (castTo)

import           Language.Javascript.JSaddle (MonadJSM)

import           Reflex
import qualified Reflex.Dom                  as Dom
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace),
                                              ElementConfig (..), HasDocument,
                                              RawElement)

import           UI.Event
import           UI.IsElement                (IsElement (..))
import           UI.Point

type PerformJS d m = ( MonadFix m
                     , MonadJSM m
                     , RawElement d ~ GHCJS.Element
                     )

type Dom t m = ( MonadFix m
               , MonadHold t m
               , TriggerEvent t m
               , PerformEvent t m
               , PostBuild t m
               , DomBuilder t m
               , DomBuilderSpace m ~ Dom.GhcjsDomSpace
               , MonadJSM m
               , PerformJS (DomBuilderSpace m) (Performable m)
               , HasDocument m
               )

newtype Element t = Element (Dom.Element EventResult Dom.GhcjsDomSpace t)

instance IsElement (Element t) where
  rawElement (Element e) = Dom._element_raw e

instance Reflex t => Dom.HasDomEvent t (Element t) en where
  type DomEventType (Element t) en = EventResultType en
  domEvent eventName (Element e) = Dom.domEvent eventName e

-- * Creating Elements

el' :: forall a m t. Dom t m
    => Text
    -> m a
    -> m (Element t, a)
el' tagName = elAttr' tagName []
{-# INLINABLE el' #-}

-- | Create and return an element with the given class.
elClass' :: forall a m t. Dom t m
         => Text
         -> Text
         -> m a
         -> m (Element t, a)
elClass' tagName class_ = elAttr' tagName [("class", class_)]
{-# INLINABLE elClass' #-}

-- | Create and return an element with the given attributes.
elAttr' :: forall a m t. Dom t m
        => Text
        -> Map Text Text
        -> m a
        -> m (Element t, a)
elAttr' tagName attr = elDynAttr' tagName (constDyn attr)
{-# INLINABLE elAttr' #-}

-- | Create and return an elment with a dynamically changing set of
-- options.
elDynAttr' :: forall a m t. Dom t m
           => Text
           -> Dynamic t (Map Text Text)
           -> m a
           -> m (Element t, a)
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
             -> m (Element t, a)
elDynAttrNs' namespace tagName attrs body = do
  modifyAttrs <- Dom.dynamicAttributesToModifyAttributes attrs
  let config = ElementConfig
        { _elementConfig_namespace = namespace
        , _elementConfig_initialAttributes = Map.empty
        , _elementConfig_modifyAttributes =
          Just $ fmapCheap Dom.mapKeysToAttributeName modifyAttrs
        , _elementConfig_eventSpec = eventSpec
        }
  (element, result) <- Dom.element tagName config body
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure (Element element, result)
  where eventSpec = Dom.GhcjsEventSpec filters handler
        filters = mempty
        handler = Dom.GhcjsEventHandler \ (eventName, event) ->
          domHandler eventName $ Dom.unGhcjsDomEvent event
{-# INLINABLE elDynAttrNs' #-}

-- * Element Properties

-- ** Bounding Rectangle

-- | The position and dimensions of a rectangle on the screen.
data Rectangle = Rectangle
  { position :: !Point
  -- ^ The x and y coordinates of the rectangle's top-left corner in
  -- px.

  , height   :: !Double
  -- ^ The height of the rectangle in px.

  , width    :: !Double
  -- ^ The width of the rectangle in px.
  }
  deriving stock (Show, Eq)

-- | Get the __bounding rectangle__ for the element.
--
-- The rectangle is positioned /relative to the viewport/.
--
-- The size of the rectangle includes the element's @padding@ and
-- @border-width@ as well as the size of the content, unless the
-- element has @box-sizing: border-box@ set, in which case the
-- rectangle only accounts for the content.
--
-- See: MDN
-- [getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
bounds :: forall e m. (IsElement e, MonadJSM m)
       => e
       -> m Rectangle
bounds (rawElement -> raw) = do
  rect <- GHCJS.getBoundingClientRect raw

  x <- GHCJS.getX rect
  y <- GHCJS.getY rect

  width  <- GHCJS.getWidth rect
  height <- GHCJS.getHeight rect

  pure Rectangle { position = Point { x, y }, width, height }

-- | The width and height of the element in px.
--
-- See: MDN
-- [getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
dimensions :: forall e m. (IsElement e, MonadJSM m)
           => e
           -> m (Double, Double)
           -- ^ The width and height of the element in px,
           -- respectively.
dimensions element = do
  Rectangle { width, height } <- bounds element
  pure (width, height)

-- | Get the position of the element /relative to the viewport/.
--
-- See: MDN
-- [getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
viewportPosition :: forall e m. (IsElement e, MonadJSM m)
                 => e
                 -> m Point
viewportPosition element = position <$> bounds element

                           -- TODO: Does this work as expected with
                           -- transforms and so on?
-- | Get the position of an element /relative to its parent element/.
--
-- For HTML elements, this uses @offsetLeft@ and @offsetTop@.
--
-- For other elements, this approximates by subtracting /this/
-- element's viewport position from its parent's viewport position (as
-- returned by 'viewportPosition').
--
-- See:
--  * [HTMLElement.offsetLeft](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetLeft)
--  * [HTMLElement.offsetLeft](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetTop)
offsetPosition :: forall e m. (IsElement e, MonadJSM m)
               => e
               -> m Point
offsetPosition element =
  castTo GHCJS.HTMLElement (rawElement element) >>= \case
    Just htmlElement -> do
      x <- GHCJS.getOffsetLeft htmlElement
      y <- GHCJS.getOffsetTop htmlElement
      pure Point { x, y }
    Nothing -> elementOffset
  where elementOffset = do
          elementPosition <- viewportPosition element
          GHCJS.getParentElement (rawElement element) >>= \case
            Nothing     -> pure elementPosition
            Just parent -> do
              parentPosition <- viewportPosition parent
              pure $ elementPosition - parentPosition
