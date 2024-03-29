-- | HTML elements.
module UI.Html
  ( Html

  , text
  , dynText

  , html
  , html'

  , div_
  , article
  , ol
  , ol_
  , ul
  , ul_

  , button
  , button'
  , label
  , labelFor

  , img
  )
where

import           Control.Lens                      ((<&>))
import           Control.Monad                     (void)

import           Data.Text                         (Text)

import           GHC.Generics                      (Generic)

import qualified GHCJS.DOM.Element                 as Element
import qualified GHCJS.DOM.Types                   as GHCJS

import           Reflex                            (Dynamic, Event, Reflex)
import qualified Reflex.Dom                        as Dom

import           UI.Attributes.AttributeSet.Reflex (AttributeSet, toDom, (=:))
import           UI.Element                        (Dom, createElement, dyn,
                                                    dynText, text)
import           UI.Element.IsElement              (FromElement (..),
                                                    IsElement (..), IsHtml (..))
import qualified UI.Event                          as Event
import           UI.Event                          (EventName (..))
import           UI.Html.Attributes                (for)
import           UI.Id                             (Id)


-- * HTML Elements

-- ** Normal HTML Elements

-- | An HTML DOM element (as opposed to an SVG element or the like).
newtype Html t = Html (Dom.Element Event.EventResult Dom.GhcjsDomSpace t)
  deriving stock (Generic)

instance FromElement Html where
  type EventResult Html = Event.EventResult
  fromElement = Html

instance IsElement (Html t) where
  rawElement (Html e) = Dom._element_raw e

instance IsHtml (Html t) where
  rawHtml (Html e) =
    GHCJS.uncheckedCastTo GHCJS.HTMLElement $ Dom._element_raw e

instance Reflex t => Dom.HasDomEvent t (Html t) en where
  type DomEventType (Html t) en = Event.EventResultType en
  domEvent eventName (Html e) = Dom.domEvent eventName e

-- * Creating HTML Elements

-- | Create an HTML element.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- html "div" [ class_ =: "draggable" ] do
--   text "Drag me!"
-- @
html :: forall a m t. (Dom t m)
     => Text
     -- ^ tag
     -> AttributeSet t
     -- ^ attributes
     -> m a
     -- ^ body
     -> m (Html t, a)
html tag attributes body = do
  (element, result) <- createElement Nothing tag (toDom attributes) body
  pure (Html element, result)
{-# INLINABLE html #-}

-- | Create an HTML element with no body.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- html' "img" [ src =: "img/example.png", alt =: "An example image." ]
-- @
html' :: forall m t. (Dom t m) => Text -> AttributeSet t -> m (Html t)
html' tag attributes = fst <$> html tag attributes (pure ())
{-# INLINABLE html' #-}

-- ** Structure

-- $ Elements for page structure: @divs@, @article@, @nav@... etc.

-- | Create a @div@ element, the generic container element in HTML.
--
-- __Example__
--
-- @
-- div_ [class_ =: ["note"]] do
--   text "When applicable, semantic elements like article are preferred over div."
-- @
div_ :: forall a m t. Dom t m
     => AttributeSet t
     -- ^ attributes
     -> m a
     -- ^ body
     -> m (Html t, a)
div_ = html "div"
{-# INLINE div_ #-}

-- | Create an @article@ element which structures self-contained
-- compositions in a document.
--
-- __Example__
--
-- @
-- blogPost title body = article [class_ =: ["post"]] do
--   h1 title
--   body
-- @
article :: forall a m t. Dom t m
        => AttributeSet t
        -- ^ attributes
        -> m a
        -- ^ body
        -> m (Html t, a)
article = html "article"
{-# INLINE article #-}

-- *** Lists

-- | An ordered list with items automatically wrapped in @li@
-- elements.
--
-- __Example__
--
-- A list with plain text entries:
--
-- @
-- ol [] (pure [text "one", text "two", text "three"])
-- @
ol :: forall a m t. Dom t m
   => AttributeSet t
   -> Dynamic t [m a]
   -- ^ Dynamic list of items.
   -> m (Html t, Event t [a])
ol attributes items = html "ol" attributes do
  dyn $ items <&> mapM (fmap snd . html "li" [])
{-# INLINABLE ol #-}

-- | An ordered list with items automatically wrapped in @li@
-- elements. Ignores the return value from creating each item.
--
-- __Example__
--
-- A list with a mix of elements as items:
--
-- @
-- ol_ [] $ pure
--   [ text "a plain text item"
--   , img [href =: "img/example.png"]
--   ]
-- @
ol_ :: forall a m t. Dom t m
    => AttributeSet t
    -> Dynamic t [m a]
    -- ^ Dynamic list of items.
    -> m (Html t)
ol_ attributes items = fst <$> html "ol" attributes do
  dyn $ mapM_ (html "li" []) <$> items
{-# INLINABLE ol_ #-}

-- | An unordered list with items automatically wrapped in @li@
-- elements.
--
-- __Example__
--
-- A list with plain text entries:
--
-- @
-- ul [] (pure [text "one", text "two", text "three"])
-- @
ul :: forall a m t. Dom t m
   => AttributeSet t
   -> Dynamic t [m a]
   -> m (Html t, Event t [a])
ul attributes items = html "ul" attributes do
  dyn $ items <&> mapM (fmap snd . html "li" [])
{-# INLINABLE ul #-}

-- | An unordered list with items automatically wrapped in @li@
-- elements. Ignores the return value from creating each item.
--
-- __Example__
--
-- A list with a mix of elements as items:
--
-- @
-- ul_ [] $ pure
--   [ text "a plain text item"
--   , img [href =: "img/example.png"]
--   ]
-- @
ul_ :: forall a m t. Dom t m
    => AttributeSet t
    -- ^ Attributes
    -> Dynamic t [m a]
    -- ^ Set of items. Can change over time.
    -> m (Html t)
ul_ attributes items = fst <$> html "ul" attributes do
  dyn $ mapM_ (html "li" []) <$> items
{-# INLINABLE ul_ #-}

-- ** Controls

-- | A pressable button.
button :: forall a m t. Dom t m
       => AttributeSet t
       -- ^ attributes
       -> m a
       -- ^ button body (often a text label)
       -> m ((Html t, Event t ()), a)
button attributes body = do
  (element, a) <- html "button" attributes body
  let pressed = void $ Dom.domEvent Click element
  pure ((element, pressed), a)

-- | A button with a static text label.
button' :: forall m t. Dom t m
        => Text
        -- ^ button label
        -> AttributeSet t
        -- ^ attributes
        -> m (Html t, Event t ())
button' t attributes = fst <$> button attributes (text t)

-- | A label that can be associated with an input or control. Clicking
-- on or tabbing to the label will activate the control.
--
-- Labels can be associated with:
--
--  * 'button'
--  * 'input'
--  * 'meter'
--  * 'output'
--  * 'progress'
--  * 'select'
--  * 'textarea'
--
-- __Examples__
--
-- Associate with an 'input' by id:
--
-- @
-- example = do
--   label [ for_ =: "username" ] (text "Username:")
--   Input.text [ id_ =: "username" ]
-- @
--
-- Associate with an 'input' by putting the input element /in/ the
-- label element:
--
-- @
-- example = label [] do
--   text "Username:"
--   Input.text []
-- @
label :: forall a m t. Dom t m
      => AttributeSet t
      -- ^ Attributes
      -> m a
      -- ^ Body
      -> m (Html t, a)
label = html "label"

-- | Shorthand for creating a text label for a given id.
--
-- __Example__
--
-- @
-- example = do
--   labelFor "username" "Username:"
--   Input.text [ id_ =: "username" ]
-- @
labelFor :: forall m t. Dom t m => Id -> Text -> m (Html t)
labelFor id_ = fmap fst . label [ for =: id_ ] . text

-- ** Media

-- | Embed an image in the page.
--
-- __Example__
--
-- @
-- img [ src =: "img/example.png", alt =: "An example image." ]
-- @
img :: forall m t. Dom t m
    => AttributeSet t
    -> m (Html t)
img = html' "img"
