-- | HTML elements.
module UI.Html
  ( Html
  , HtmlInput

  , html
  , html'

  , div_
  , article
  , ol
  , ol_
  , ul
  , ul_

  , Button (..)
  , button
  , button'
  , label

  , img
  )
where

import           Control.Monad        (void)

import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           GHC.Generics         (Generic)
import           GHC.TypeLits         (KnownSymbol, symbolVal)

import qualified GHCJS.DOM.Element    as Element
import qualified GHCJS.DOM.Types      as GHCJS

import           Reflex               (Event, Reflex)
import qualified Reflex.Dom           as Dom

import           UI.Attributes        (AttributeSet, toDom)
import           UI.Element           (Dom, createElement, text)
import           UI.Element.IsElement (FromElement (..), IsElement (..),
                                       IsHtml (..), IsHtmlInput (..))
import qualified UI.Event             as Event
import           UI.Event             (EventName (..))


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

-- ** Input Elements

-- | An HTML DOM /input/ element (ie @HTMLInputElement@ in
-- JavaScript).
newtype HtmlInput t = HtmlInput (Dom.Element Event.EventResult Dom.GhcjsDomSpace t)
  deriving stock (Generic)

instance FromElement HtmlInput where
  type EventResult HtmlInput = Event.EventResult
  fromElement = HtmlInput

instance IsElement (HtmlInput t) where
  rawElement (HtmlInput e) = Dom._element_raw e

instance IsHtml (HtmlInput t) where
  rawHtml (HtmlInput e) =
    GHCJS.uncheckedCastTo GHCJS.HTMLElement $ Dom._element_raw e

instance IsHtmlInput (HtmlInput t) where
  rawHtmlInput (HtmlInput e) =
    GHCJS.uncheckedCastTo GHCJS.HTMLInputElement $ Dom._element_raw e

instance Reflex t => Dom.HasDomEvent t (HtmlInput t) en where
  type DomEventType (HtmlInput t) en = Event.EventResultType en
  domEvent eventName (HtmlInput e) = Dom.domEvent eventName e

-- * Creating HTML Elements

-- | Create an HTML element.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- html @"div" [ class_ =: "draggable" ] do
--   text "Drag me!"
-- @
html :: forall element a m t. (KnownSymbol element, Dom t m)
     => AttributeSet t element "HTML"
     -- ^ attributes
     -> m a
     -- ^ body
     -> m (Html t, a)
html = createElement Nothing tag . toDom
  where tag = Text.pack $ symbolVal (Proxy :: Proxy element)
{-# INLINABLE html #-}

-- | Create an HTML element with no body.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- html' @"img" [ src =: "img/example.png", alt =: "An example image." ]
-- @
html' :: forall element m t. (KnownSymbol element, Dom t m)
      => AttributeSet t element "HTML"
      -> m (Html t)
html' attributes = fst <$> html attributes (pure ())
{-# INLINABLE html' #-}

-- ** Structure

-- $ Elements for page structure: @divs@, @article@, @nav@... etc.

-- | Create a @div@ element, the generic container element in HTML.
--
-- __Example__
--
-- @
-- div_ [class_ =: "note"] do
--   text "When applicable, semantic elements like article are preferred over div."
-- @
div_ :: forall a m t. Dom t m
     => AttributeSet t "div" "HTML"
     -- ^ attributes
     -> m a
     -- ^ body
     -> m (Html t, a)
div_ = html
{-# INLINE div_ #-}

-- | Create an @article@ element which structures self-contained
-- compositions in a document.
--
-- __Example__
--
-- @
-- blogPost title body = article [class_ =: "post"] do
--   h1 title
--   body
-- @
article :: forall a m t. Dom t m
        => AttributeSet t "article" "HTML"
        -- ^ attributes
        -> m a
        -- ^ body
        -> m (Html t, a)
article = html
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
-- ol [] ["one", "two", "three"]
-- @
ol :: forall a m t. Dom t m
   => AttributeSet t "ol" "HTML"
   -> [m a]
   -> m (Html t, [a])
ol attributes = html attributes . mapM (fmap snd . html @"li" [])
{-# INLINABLE ol #-}

-- | An ordered list with items automatically wrapped in @li@
-- elements. Ignores the return value from creating each item.
--
-- __Example__
--
-- A list with a mix of elements as items:
--
-- @
-- ol_ []
--   [ "a plain text item"
--   , img [href =: "img/example.png"]
--   ]
-- @
ol_ :: forall a m t. Dom t m
    => AttributeSet t "ol" "HTML"
    -> [m a]
    -> m (Html t)
ol_ attributes = fmap fst . html attributes . mapM_ (html @"li" [])

{-# INLINABLE ol_ #-}

-- | An unordered list with items automatically wrapped in @li@
-- elements.
--
-- __Example__
--
-- A list with plain text entries:
--
-- @
-- ul [] ["one", "two", "three"]
-- @
ul :: forall a m t. Dom t m
   => AttributeSet t "ul" "HTML"
   -> [m a]
   -> m (Html t, [a])
ul attributes = html attributes . mapM (fmap snd . html @"li" [])
{-# INLINABLE ul #-}

-- | An unordered list with items automatically wrapped in @li@
-- elements. Ignores the return value from creating each item.
--
-- __Example__
--
-- A list with a mix of elements as items:
--
-- @
-- ul_ []
--   [ "a plain text item"
--   , img [href =: "img/example.png"]
--   ]
-- @
ul_ :: forall a m t. Dom t m
    => AttributeSet t "ul" "HTML"
    -> [m a]
    -> m (Html t)
ul_ attributes = fmap fst . html attributes . mapM_ (html @"li" [])
{-# INLINABLE ul_ #-}

-- ** Controls

-- | A button element. Has the underlying HTML element for the button
-- as well as an event that fires when the button is pressed.
data Button t = Button
  { element :: Html t
  , pressed :: Event t ()
  }
  deriving stock (Generic)

-- | A pressable button.
button :: forall a m t. Dom t m
       => AttributeSet t "button" "HTML"
       -- ^ attributes
       -> m a
       -- ^ button body (often a text label)
       -> m (Button t, a)
button attributes body = do
  (element, a) <- html attributes body
  let pressed = void $ Dom.domEvent Click element
  pure (Button { element, pressed }, a)

-- | A button with a static text label.
button' :: forall a m t. Dom t m
        => Text
        -- ^ button label
        -> AttributeSet t "button" "HTML"
        -- ^ attributes
        -> m (Button t)
button' label attributes = fst <$> button attributes do
  text label

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
--   labelFor "username" [] (text "Username:")
--   input [ type_ =: Text, id_ =: "username" ]
-- @
--
-- Associate with an 'input' by putting the input element /in/ the
-- label element:
--
-- @
-- example = label [] (text "Username:") do
--   input [ type_ =: Text ]
-- @
label :: forall a m t. Dom t m
      => AttributeSet t "label" "HTML"
      -- ^ Attributes
      -> m a
      -- ^ Body
      -> m (Html t, a)
label = html

-- ** Media

-- | Embed an image in the page.
--
-- __Example__
--
-- @
-- img [ src =: "img/example.png", alt =: "An example image." ]
-- @
img :: forall m t. Dom t m
    => AttributeSet t "img" "HTML"
    -> m (Html t)
img = html'
