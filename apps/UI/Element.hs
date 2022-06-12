module UI.Element
  ( PerformJS
  , Dom

  , text
  , createElement

  , getAttribute
  , setAttribute

  , Rectangle (..)
  , overlap
  , area

  , bounds
  , dimensions
  , viewportPosition
  , offsetPosition
  )
where

import           Control.Monad.Fix           (MonadFix)

import           Data.Hashable               (Hashable)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)

import           GHC.Generics                (Generic)

import qualified GHCJS.DOM.DOMRect           as GHCJS
import qualified GHCJS.DOM.Element           as Element
import qualified GHCJS.DOM.HTMLElement       as HTMLElement
import qualified GHCJS.DOM.Node              as Node
import qualified GHCJS.DOM.Types             as GHCJS

import           Language.Javascript.JSaddle (MonadJSM)

import           Linear                      (V2 (..))

import qualified Reflex
import           Reflex                      (Dynamic)
import qualified Reflex.Dom                  as Dom
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace),
                                              ElementConfig (..), HasDocument,
                                              RawElement)

import qualified UI.Event                    as Event
import           UI.IsElement                (FromElement (..), IsElement (..))

type PerformJS d m = ( MonadFix m
                     , MonadJSM m
                     , RawElement d ~ Element.Element
                     )

type Dom t m = ( MonadFix m
               , Reflex.MonadHold t m
               , Reflex.TriggerEvent t m
               , Reflex.PerformEvent t m
               , Reflex.PostBuild t m
               , DomBuilder t m
               , DomBuilderSpace m ~ Dom.GhcjsDomSpace
               , MonadJSM m
               , PerformJS (DomBuilderSpace m) (Reflex.Performable m)
               , HasDocument m
               )

-- | Create a text node with no element.
text :: forall m t. Dom t m => Text -> m ()
text = Dom.text

-- | Create an element with an (optional) namespace.
--
-- This is the main interface between this library's structured
-- element type and the underlying @reflex-dom@ machinery.
createElement :: forall e a m t. (Dom t m, FromElement e, EventResult e ~ Event.EventResult)
              => Maybe Text
              -- ^ Optional namespace
              -> Text
              -- ^ Tag
              -> Dynamic t (Map Text Text)
              -- ^ Dynamic attributes
              -> m a
              -- ^ Body
              -> m (e t, a)
createElement namespace tagName attrs body = do
  modifyAttrs <- Dom.dynamicAttributesToModifyAttributes attrs
  let config = ElementConfig
        { _elementConfig_namespace = namespace
        , _elementConfig_initialAttributes = Map.empty
        , _elementConfig_modifyAttributes =
          Just $ Reflex.fmapCheap Dom.mapKeysToAttributeName modifyAttrs
        , _elementConfig_eventSpec = eventSpec
        }
  (element, result) <- Dom.element tagName config body
  postBuild <- Reflex.getPostBuild
  Reflex.notReadyUntil postBuild
  pure (fromElement element, result)
  where eventSpec = Dom.GhcjsEventSpec filters handler
        filters = mempty
        handler = Dom.GhcjsEventHandler \ (eventName, event) ->
          Event.domHandler eventName $ Dom.unGhcjsDomEvent event
{-# INLINABLE createElement #-}

-- * Element Properties

-- ** Current attributes

-- | Get the current value of an attribute on the element.
--
-- Will be 'Nothing' if the attribute is not set.
getAttribute :: forall e m. (MonadJSM m, IsElement e)
             => Text
             -- ^ Name of the attribute
             -> e
             -- ^ Element
             -> m (Maybe Text)
getAttribute attribute (rawElement -> element) = do
  hasAttribute <- Element.hasAttribute element attribute
  if hasAttribute then pure Nothing else do
    Element.getAttribute element attribute

setAttribute :: forall e m. (MonadJSM m, IsElement e)
             => Text
                -- ^ Name of the attribute to set.
             -> Maybe Text
                -- ^ New value of the attribute or 'Nothing' to remove
                -- the attribute.
             -> e
               -- ^ Element
             -> m ()
setAttribute attribute value (rawElement -> element) = case value of
  Nothing -> Element.removeAttribute element attribute
  Just v  -> Element.setAttribute element attribute v

-- ** Bounding Rectangle

-- | The position and dimensions of a rectangle on the screen.
data Rectangle = Rectangle
  { position :: !(V2 Double)
  -- ^ The x and y coordinates of the rectangle's top-left corner in
  -- px.

  , width    :: !Double
  -- ^ The width of the rectangle in px.

  , height   :: !Double
  -- ^ The height of the rectangle in px.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | Get the parts of two rectangles that overlap—that is, the
-- intersection of two rectangles.
--
-- Returns 'Nothing' if the rectangles do not overlap.
--
-- >>> let r₁ = Rectangle (V2 5 10) 20 30
-- >>> let r₂ = Rectangle (V2 10 15) 16 20
-- >>> overlap r₁ r₂
-- Just (Rectangle {position = V2 10.0 15.0, width = 15.0, height = 20.0})
--
-- >>> let r₁ = Rectangle (V2 5 10) 20 30
-- >>> let r₂ = Rectangle (V2 26 15) 16 20
-- >>> overlap r₁ r₂
-- Nothing
overlap :: Rectangle -> Rectangle -> Maybe Rectangle
overlap (Rectangle (V2 x₁ y₁) w₁ h₁) (Rectangle (V2 x₂ y₂) w₂ h₂) =
  [ Rectangle { position = V2 x y, height, width } | doOverlap ]
  where x = max x₁ x₂
        y = max y₁ y₂
        width = min (x₁ + w₁) (x₂ + w₂) - x
        height = min (y₁ + h₁) (y₂ + h₂) - y

        doOverlap = width > 0 && height > 0

-- | Return the area of the rectangle.
area :: Rectangle -> Double
area Rectangle { width, height } = width * height


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
  rect <- Element.getBoundingClientRect raw

  x <- GHCJS.getX rect
  y <- GHCJS.getY rect

  width  <- GHCJS.getWidth rect
  height <- GHCJS.getHeight rect

  pure Rectangle { position = V2 x y, width, height }

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
                 -- ^ element
                 -> m (V2 Double)
                 -- ^ viewport position
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
               -> m (V2 Double)
offsetPosition element =
  GHCJS.castTo GHCJS.HTMLElement (rawElement element) >>= \case
    Just htmlElement -> do
      x <- HTMLElement.getOffsetLeft htmlElement
      y <- HTMLElement.getOffsetTop htmlElement
      pure (V2 x y)
    Nothing -> elementOffset
  where elementOffset = do
          elementPosition <- viewportPosition element
          Node.getParentElement (rawElement element) >>= \case
            Nothing     -> pure elementPosition
            Just parent -> do
              parentPosition <- viewportPosition parent
              pure $ elementPosition - parentPosition
