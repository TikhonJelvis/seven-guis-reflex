{-# LANGUAGE UndecidableInstances #-}
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

import qualified Data.Colour              as Colour
import           Data.Default.Class       (def)
import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))
import qualified Data.Text                as Text
import           Data.Time                (Day, LocalTime, TimeOfDay)
import qualified Data.Time.Format.ISO8601 as Time
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector

import           GHC.Generics             (Generic)
import           GHC.TypeLits             (KnownSymbol, symbolVal)

import qualified GHCJS.DOM.Types          as GHCJS

import           Reflex                   (Dynamic, Event, Reflex)
import qualified Reflex.Dom               as Dom

import           UI.Attributes            (AttributeSet, boolean,
                                           fromAttributeValue, isHtmlWhitespace,
                                           native, override, toAttributeValue,
                                           toDom, (=:))
import           UI.Color                 (Opaque (..))
import           UI.Element               (Dom, InputConfig (..),
                                           createInputElement)
import           UI.Element.IsElement     (IsElement (..), IsHtml (..),
                                           IsHtmlInput (..))
import           UI.Email                 (Email, validate)
import qualified UI.Event                 as Event
import           UI.Html.Attributes       ()

-- * Input Elements

-- $ These different elements are all created with the @input@ tag and
-- represented as @HTMLInputElement@ in JavaScript.
--
-- Note: some properties and methods of @HTMLInputElement@ only make
-- sense for specific types of inputs, which is worth keeping in mind
-- if you're interfacing with the DOM object directly through
-- 'IsHtmlInput'.

-- | An HTML DOM /input/ element (ie @HTMLInputElement@ in
-- JavaScript).
newtype HtmlInput t = HtmlInput
  (Dom.InputElement Event.EventResult Dom.GhcjsDomSpace t)
  deriving stock (Generic)
  deriving newtype (Dom.HasValue)

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
      -- ^ Config for setting the input value externally.
      -> AttributeSet t type_ "HTML"
      -- ^ Attributes
      -> m (HtmlInput t)
input config attributes = do
  element <- createInputElement Nothing domAttributes config
  pure $ HtmlInput element
  where domAttributes =
          toDom $ attributes <> [native @'["HTML"] "type" =: type_]
        -- NOTE: using native rather than override so that "type" can
        -- always be overriden by the caller

        type_ = Text.pack $ symbolVal (Proxy @type_)
{-# INLINABLE input #-}

-- ** Types of Inputs

        -- TODO: support "intermediate" state checkboxes
-- | Create a checkbox.
--
-- __Examples__
--
-- A checkbox that is checked when the page loads:
--
-- @
-- checkbox [ checked =: True ] never
-- @
--
-- Note: the @checked@ attribute only affects the /initial/ state of
-- the checkbox. Setting the attribute dynamically will /not/ cause
-- the checkbox to be set and querying the @checked@ attribute will
-- not necessarily reflect the current state of the checkbox.
--
-- Tracking the value of checkboxes:
--
-- @
-- ingredient name = do
--   snd <$> label [] do
--     text name
--     checked <- snd <$> checkbox [] never
--     pure $ bool Nothing (Just name) <$> checked
--
-- ingredients = catMaybes . snd <$> ul []
--   [ ingredient "onions"
--   , ingredient "tomatoes"
--   , ingredient "salt"
--   ]
-- @
checkbox :: forall m t. Dom t m
         => AttributeSet t "checkbox" "HTML"
         -> Event t Bool
         -- ^ Set the state of the checkbox explicitly. 'True':
         -- checked, 'False': unchecked.
         --
         -- Use 'Reflex.never' if you never want to override the value
         -- explicitly.
         -> m (HtmlInput t, Dynamic t Bool)
         -- ^ The element + current state (True = checked, False =
         -- unchecked)
checkbox attributes setChecked = do
  HtmlInput e <- input def { setChecked = Just setChecked  } attributes
  pure (HtmlInput e, Dom._inputElement_checked e)
           -- TODO: does this always force the checkbox to start
           -- unchecked?

-- | A button that opens a color-picker and lets the user specify a
-- color.
--
-- Only supports opaque colors and doesn't recognize color names.
--
-- If the @value@ of a color input does not parse as a valid color (in
-- the @#xxxxxx@ format), the value is interpreted as @#000000@
-- (black).
--
-- __Example__
--
-- A color picker that starts out picking red:
--
-- @
-- color [ color_value =: Opaque Colour.red ] never
-- @
color :: forall m t. Dom t m
      => AttributeSet t "color" "HTML"
      -- ^ Attributes
      -> Event t Opaque
      -- ^ Set the picked color. Use 'never' if you don't want to
      -- explicitly override the value.
      -> m (HtmlInput t, Dynamic t Opaque)
      -- ^ The element and the current picked color
color attributes setColor = do
  e <- input def { setValue = Just $ toAttributeValue <$> setColor } attributes
  pure (e, getColor <$> Dom.value e)
  where getColor = fromMaybe (Opaque Colour.black) . fromAttributeValue

-- | A month + year input.
--
-- If the value is not in @YYYY-MM@ format, it is treated as
-- 'Nothing'.
--
-- __Example__
--
-- A month input with a default value:
--
-- @
-- month [ month_value =: (2022, 12) ] never
-- @
month :: forall m t. Dom t m
      => AttributeSet t "month" "HTML"
      -- ^ Attributes
      -> Event t (Integer, Int)
      -- ^ Explicitly set the year-month value of the input. Use
      -- 'Reflex.never' if you don't need this.
      -> m (HtmlInput t, Dynamic t (Maybe (Integer, Int)))
      -- ^ The element and the current year-month pair if it parses.
month attributes setMonth = do
  e <- input def { setValue = Just $ toText <$> setMonth } attributes
  pure (e, getMonth <$> Dom.value e)
  where toText = Text.pack . Time.formatShow Time.yearMonthFormat
        getMonth = Time.formatParseM Time.yearMonthFormat . Text.unpack

-- | A date picker.
--
-- If the value is not in @YYYY-MM-DD@ format, it is treated as
-- 'Nothing'.
--
-- __Example__
--
-- A date picker with an initial value of @2022-12-23@:
--
-- @
-- date [ date_value =: read "2022-12-23" ] never
-- @
date :: forall m t. Dom t m
     => AttributeSet t "date" "HTML"
     -> Event t Day
     -> m (HtmlInput t, Dynamic t (Maybe Day))
date attributes setDate = do
  e <- input def { setValue = Just $ toText <$> setDate } attributes
  pure (e, getDay <$> Dom.value e)
  where toText = Text.pack . Time.iso8601Show
        getDay = Time.iso8601ParseM . Text.unpack

-- | A time picker.
--
-- The value has to be in the @hh:mm@ or @hh:mm:ss@ format.
--
-- __Example__
--
-- A time picker with an initial value:
--
-- @
-- time [ time_value =: read "14:25" ] never
-- @
time :: forall m t. Dom t m
     => AttributeSet t "time" "HTML"
     -- ^ Attributes.
     -> Event t TimeOfDay
     -- ^ Set the value explicitly. Use 'never' if you don't need
     -- this.
     -> m (HtmlInput t, Dynamic t (Maybe TimeOfDay))
     -- ^ The input element as well as the time of day if it parses
     -- correctly.
time attributes setTime = do
  e <- input def { setValue = Just $ toAttributeValue <$> setTime } attributes
  pure (e, fromAttributeValue <$> Dom.value e)

-- | A date + time picker.
--
-- If the underlying value is not in a valid format, the value is
-- 'Nothing'.
--
-- __Example__
--
-- Date-picker with initial value set:
--
-- @
-- datetime [ datetime_value =: read "2022-12-23 14:23:00" ] never
-- @
datetime :: forall m t. Dom t m
         => AttributeSet t "datetime-local" "HTML"
         -- ^ Attributes
         -> Event t LocalTime
         -- ^ Explicitly set the value of the date picker. Use
         -- @never@ if you don't ever want to do this.
         -> m (HtmlInput t, Dynamic t (Maybe LocalTime))
datetime attributes setDatetime = do
  e <- input def { setValue = Just $ toAttributeValue <$> setDatetime } attributes
  pure (e, fromAttributeValue <$> Dom.value e)

-- | An email address input.
--
-- If the email address entered is invalid, the value is 'Nothing'.
--
-- Note: setting the @"multiple"@ attribute on this element will
-- result in 'Nothing' for anything except exactly one entry. For
-- allowing multiple emails, use 'emails'.
--
-- __Examples__
--
-- Email address with initial value set:
--
-- @
-- email [ email_value =: "john.doe@example.com" ]
-- @
email :: forall m t. Dom t m
      => AttributeSet t "email" "HTML"
      -> Event t Email
      -> m (HtmlInput t, Dynamic t (Maybe Email))
email attributes setEmail = do
  e <- input def { setValue = Just $ toAttributeValue <$> setEmail } attributes
  pure (e, fromAttributeValue <$> Dom.value e)

-- | An email address input that allows multiple email addresses.
--
-- Any invalid emails in the result will be 'Nothing'.
--
-- __Examples__
--
-- A field supporting multiple email addresses with two set as the
-- initial value:
--
-- @
-- email [ emails_value =: ["a@example.com", "b@example.com"] ]
-- @
emails :: forall m t. (Dom t m)
       => AttributeSet t "emails" "HTML"
       -> Event t (Vector Email)
       -> m (HtmlInput t, Dynamic t (Vector (Maybe Email)))
emails attributes setEmails = do
  e <- input def { setValue = Just $ toAttributeValue <$> setEmails } attributes'
  pure (e, getEmails <$> Dom.value e)
  where attributes' :: AttributeSet t "emails" "HTML"
        attributes' = attributes <>
          [ boolean @'["emails"] "multiple" =: True
          , override @'["emails"] "type" =: "email"
          ]
        getEmails = Vector.fromList . map validate .
          filter (Text.all isHtmlWhitespace) . Text.split (== ',')
