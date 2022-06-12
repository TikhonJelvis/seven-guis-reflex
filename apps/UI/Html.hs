-- | HTML elements.
module UI.Html where

import           Data.Proxy           (Proxy (..))
import qualified Data.Text            as Text

import           GHC.TypeLits         (KnownSymbol, symbolVal)

import qualified GHCJS.DOM.Element    as Element
import qualified GHCJS.DOM.Types      as GHCJS

import           Reflex               (Reflex)
import qualified Reflex.Dom           as Dom

import           UI.Attributes        (AttributeSet, toDom)
import           UI.Element           (Dom, createElement)
import           UI.Element.IsElement (FromElement (..), IsElement (..),
                                       IsHtml (..), IsHtmlInput (..))
import qualified UI.Event             as Event


-- * HTML Elements

-- | An HTML DOM element (as opposed to an SVG element or the like).
newtype Html t = Html (Dom.Element Event.EventResult Dom.GhcjsDomSpace t)

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

-- ** Creating HTML Elements

-- | Create an HTML element.
--
-- If the @element@ type variable is ambiguous, you can specify it
-- with a type application.
--
-- __Example__
--
-- @
-- html @"div" [class_ =: "draggable"] (pure ())
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

-- *** Structure

-- $ Elements for page structure: @divs@, @article@, @nav@... etc.

-- | Create a @div@ element, the generic container element in HTML.
--
-- __Example__
--
-- @
-- div [class_ =: "note"] do
--   text "When applicable, semantic elements like article are preferred over div."
-- @
div :: forall a m t. Dom t m
    => AttributeSet t "div" "HTML"
    -- ^ attributes
    -> m a
    -- ^ body
    -> m (Html t, a)
div = html
{-# INLINE div #-}

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

-- * Input Elements

-- | An HTML DOM /input/ element (ie @HTMLInputElement@ in
-- JavaScript).
newtype HtmlInput t = HtmlInput (Dom.Element Event.EventResult Dom.GhcjsDomSpace t)

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
