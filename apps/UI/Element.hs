{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI.Element where

import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (runReaderT)

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
                                              ElementConfig (..), RawElement)

import           UI.Event
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
               )

-- * Creating Elements

el' :: forall a m t. Dom t m
    => Text
    -> m a
    -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
el' tagName = elAttr' tagName []
{-# INLINABLE el' #-}

-- | Create and return an element with the given class.
elClass' :: forall a m t. Dom t m
         => Text
         -> Text
         -> m a
         -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
elClass' tagName class_ = elAttr' tagName [("class", class_)]
{-# INLINABLE elClass' #-}

-- | Create and return an element with the given attributes.
elAttr' :: forall a m t. Dom t m
        => Text
        -> Map Text Text
        -> m a
        -> m (Dom.Element EventResult (DomBuilderSpace m) t, a)
elAttr' tagName attr = elDynAttr' tagName (constDyn attr)
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
elDynAttrNs' namespace tagName attrs body = do
  modifyAttrs <- Dom.dynamicAttributesToModifyAttributes attrs
  let config = ElementConfig
        { _elementConfig_namespace = namespace
        , _elementConfig_initialAttributes = Map.empty
        , _elementConfig_modifyAttributes =
          Just $ fmapCheap Dom.mapKeysToAttributeName modifyAttrs
        , _elementConfig_eventSpec = eventSpec
        }
  result <- Dom.element tagName config body
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result
  where eventSpec = Dom.GhcjsEventSpec filters handler
        filters = mempty
        handler = Dom.GhcjsEventHandler \ (eventName, event) ->
          runReaderT (domHandler eventName) (Dom.unGhcjsDomEvent event)
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
bounds :: forall m d er t. PerformJS d m
       => Dom.Element er d t
       -> m Rectangle
bounds element = do
  rect <- GHCJS.getBoundingClientRect raw

  x <- GHCJS.getX rect
  y <- GHCJS.getY rect

  width  <- GHCJS.getWidth rect
  height <- GHCJS.getHeight rect

  pure Rectangle { position = Point { x, y }, width, height }
  where raw = Dom._element_raw element

-- | The width and height of the element in px.
--
-- See: MDN
-- [getBoundingClientRect](https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect)
dimensions :: forall m d er t. PerformJS d m
           => Dom.Element er d t
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
viewportPosition :: forall m d er t. PerformJS d m
                 => Dom.Element er d t
                 -> m Point
viewportPosition element = position <$> bounds element

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
offsetPosition :: forall m d er t. PerformJS d m
               => Dom.Element er d t
               -> m Point
offsetPosition element = castTo GHCJS.HTMLElement raw >>= \case
  Just htmlElement -> do
    x <- GHCJS.getOffsetLeft htmlElement
    y <- GHCJS.getOffsetTop htmlElement
    pure Point { x, y }
  Nothing -> elementOffset
  where raw = Dom._element_raw element

        elementOffset = do
          elementPosition <- viewportPosition element
          GHCJS.getParentElement raw >>= \case
            Nothing -> pure elementPosition
            Just parent -> do
              parentRect <- GHCJS.getBoundingClientRect parent

              parentX <- GHCJS.getX parentRect
              parentY <- GHCJS.getY parentRect

              pure $ elementPosition - Point { x = parentX, y = parentY }
