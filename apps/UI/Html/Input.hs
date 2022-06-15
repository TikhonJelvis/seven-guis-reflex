-- | HTML controls and input widgets: @select@, @textarea@ and
-- different types of @input@.
--
-- HTML provides different types of input widgets like checkboxes,
-- text fields, range sliders and radio buttons. Some input widgets
-- (like @select@ and @textarea@) have their own tags, but most are
-- created with the @input@ tag by specifying different @type@
-- attributes. These are technically the same element—represented by
-- @HTMLInputElement@ in JavaScript—but they support different
-- attributes and behave differently, so we use the @type@ of an input
-- as its tag at the type level:
--
-- @
-- text :: AttributeSet t "text" "HTML"
-- radio :: AttributeSet t "radio" "HTML"
-- range :: AttributeSet t "range" "HTML"
-- ...
-- @
module UI.Html.Input where
import           Data.Proxy           (Proxy (..))
import qualified Data.Text            as Text

import           GHC.Generics         (Generic)
import           GHC.TypeLits         (KnownSymbol, symbolVal)

import qualified GHCJS.DOM.Types      as GHCJS

import           Reflex               (Reflex)
import qualified Reflex.Dom           as Dom

import           UI.Attributes        (AttributeSet, native, toDom, (=:))
import           UI.Element           (Dom, InputConfig, createInputElement)
import           UI.Element.IsElement (IsElement (..), IsHtml (..),
                                       IsHtmlInput (..))
import qualified UI.Event             as Event

-- ** Input Elements

-- $ These different elements are all created with the @input@ tag and
-- represented as @HTMLInputElement@ in JavaScript.
--
-- Note: some properties and methods of @HTMLInputElement@ only make
-- sense for specific types of inputs, which is worth keeping in mind
-- if you're interfacing with the DOM object directly through
-- 'IsHtmlInput'.

-- | An HTML DOM /input/ element (ie @HTMLInputElement@ in
-- JavaScript).
newtype HtmlInput t = HtmlInput (Dom.InputElement Event.EventResult Dom.GhcjsDomSpace t)
  deriving stock (Generic)

instance IsElement (HtmlInput t) where
  rawElement (HtmlInput e) =
    Dom._element_raw $ Dom._inputElement_element e

instance IsHtml (HtmlInput t) where
  rawHtml = GHCJS.uncheckedCastTo GHCJS.HTMLElement . rawElement

instance IsHtmlInput (HtmlInput t) where
  rawHtmlInput = GHCJS.uncheckedCastTo GHCJS.HTMLInputElement . rawElement

instance Reflex t => Dom.HasDomEvent t (HtmlInput t) en where
  type DomEventType (HtmlInput t) en = Event.EventResultType en
  domEvent eventName (HtmlInput e) =
    Dom.domEvent eventName $ Dom._inputElement_element e

-- | Create an HTML input element.
--
-- The element /tag/ will always be @input@ and the @type@ attribute
-- will be explicitly set to the tag in the 'AttributeSet''s type.
--
-- Note: Input elements never take children.
--
-- __Example__
--
-- A password field for entering a numeric PIN:
--
-- @
-- input @"password" [ inputmode =: Numeric ]
-- @
--
-- If you need to leave the tag attribute out or set it dynamically,
-- specify @"input"@ instead. This will allow using attributes for
-- /any/ type of input with the element.
--
-- @
-- input @"input" [ override "type" =: "foo" ]
-- @
input :: forall type_ m t. (KnownSymbol type_, Dom t m)
      => InputConfig t
      -> AttributeSet t type_ "HTML"
      -> m (HtmlInput t)
input config attributes = HtmlInput <$>
  createInputElement Nothing (toDom $ attributes <> [input_type =: type_]) config
  where type_ = Text.pack $ symbolVal (Proxy @type_)
        input_type = native @'["HTML"] "type"
{-# INLINABLE input #-}
