{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
module UI.Html.Input
  ( HtmlInput (..)
  , Inputs
  , input
  , input'

  , text
  , tel
  , search
  , url
  , password
  , email
  , emails

  , checkbox

  , number
  , integer
  , range
  , integerRange

  , month
  , week
  , date
  , time
  , datetime

  , color

  , Enabled (..)
  , enabledIf
  , enabled

  , checked

  , minlength
  , maxlength

  , value
  , url_value
  , color_value
  , email_value
  , emails_value

  , number_value
  , number_min
  , number_max
  , number_step

  , integer_value
  , integer_min
  , integer_max
  , integer_step

  , week_value
  , month_value
  , date_value
  , time_value
  , datetime_value
  )
where

import           Data.Bool                         (bool)
import qualified Data.Colour                       as Colour
import           Data.Default.Class                (def)
import           Data.Hashable                     (Hashable)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Time                         (Day, LocalTime, TimeOfDay)
import qualified Data.Time.Format.ISO8601          as Time
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector

import           GHC.Generics                      (Generic)
import           GHC.TypeLits                      (KnownSymbol)

import qualified GHCJS.DOM.Types                   as GHCJS

import qualified Reflex
import           Reflex                            (Dynamic, Event, Reflex)
import qualified Reflex.Dom                        as Dom

import           UI.Attributes                     (AsAttributeValue, Attribute,
                                                    boolean, fromAttributeValue,
                                                    isHtmlWhitespace, logical,
                                                    native, toAttributeValue,
                                                    (=.))
import qualified UI.Attributes.AttributeSet.Reflex as AttributeSet
import           UI.Attributes.AttributeSet.Reflex (AttributeSet, toDom, (=:))
import           UI.Color                          (Opaque (..))
import           UI.Element                        (Dom, InputConfig (..),
                                                    createInputElement)
import           UI.Element.IsElement              (IsElement (..), IsHtml (..),
                                                    IsHtmlInput (..))
import           UI.Email                          (Email, validate)
import qualified UI.Event                          as Event
import           UI.Password                       (Password (..))
import           UI.Type.List                      (KnownSymbols, type (<>))
import           UI.Url                            (Url (..))

-- * Input Elements

-- $ These different elements are all created with the @input@ tag and
-- represented as @HTMLInputElement@ in JavaScript.
--
-- Note: some properties and methods of @HTMLInputElement@ only make
-- sense for specific types of inputs, which is worth keeping in mind
-- if you're interfacing with the DOM object directly through
-- 'IsHtmlInput'.

-- | An HTML DOM @input@ element (@HTMLInputElement@ in JavaScript).
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

    -- TODO: figure out better way of managing grouping/subtyping like
    -- this?
-- | Every type of input defined in this module.
type Inputs = ["input"
              , "text"
              , "tel"
              , "search"
              , "url"
              , "password"
              , "email"
              , "emails"
              , "checkbox"
              , "radio"
              , "number"
              , "integer"
              , "month"
              , "week"
              , "date"
              , "time"
              , "datetime"
              , "color"
              ]

-- | Create an HTML input element.
--
-- Input elements never take children.
--
-- Because different types of inputs behave differently and accept
-- different attributes, we track the kind of input in the type rather
-- than the @"input"@ tag directly: for example, the type of 'integer'
-- has the tag @"integer"@ rather than @"input"@.
--
-- @
-- integer :: AttributeSet t "integer" "HTML" -> {- ... -}
-- @
--
-- This lets us specialize the type of attributes like 'integer_value'
-- only to types of inputs where they make sense.
--
-- However, all input-specific attributes /still/ support the
-- @"input"@ tag. If you need an input element where the @type@ is set
-- dynamically, you can tag it with @"input"@ and use /any/ of the
-- specialized input attributes with it:
--
-- @
-- myInput type_ = input @"input" type_ def [ integer_value =: 10 ]
-- @
--
-- Warning: if you specify multiple versions of the same underlying
-- attribute (eg @integer_value@ /and/ @url_value@), which value gets
-- used is implementation-defined.
--
-- __Example__
--
-- A password field for entering a numeric PIN:
--
-- @
-- input @"password" def [ inputmode =: Numeric ]
-- @
input :: forall type_ m t. (KnownSymbol type_, Dom t m)
      => Text
      -- ^ Setting for the @type@ attribute.
      -> AttributeSet t type_ "HTML"
      -- ^ Attributes
      -> InputConfig t
      -- ^ Config for setting the input value externally.
      -> m (HtmlInput t)
input type_ attributes config = do
  element <- createInputElement Nothing domAttributes config
  pure $ HtmlInput element
  where domAttributes =
          toDom $ attributes <> [native @'["HTML"] "type" =: type_]
        -- NOTE: using native rather than override so that "type" can
        -- always be overriden by the caller
{-# INLINABLE input #-}

-- | A version of 'input' that automatically handles converting
-- to/from types with 'AsAttributeValue' instances.
input' :: forall type_ a m t. (KnownSymbol type_, Dom t m, AsAttributeValue a)
       => Text
       -- ^ Setting for the @type@ attribute
       -> AttributeSet t type_ "HTML"
       -- ^ Attributes
       -> Event t a
       -- ^ Explicitly override the current value. Use 'never' if you
       -- don't need this.
       -> m (HtmlInput t, Dynamic t (Maybe a))
       -- ^ The input element as well as its current value, or
       -- 'Nothing' if it does not parse.
input' type_ attributes setValue = do
  let config = def { setValue = Just $ toAttributeValue <$> setValue }
  e <- input type_ attributes config
  pure (e, fromAttributeValue <$> Dom.value e)

-- ** Text-Based Inputs

-- | A plain text input field.
--
-- __Examples__
--
-- Text input with an initial value:
--
-- @
-- text [ value =: "initial value" ] never
-- @
text :: forall m t. Dom t m
     => AttributeSet t "text" "HTML"
     -- ^ Attributes
     -> Event t Text
     -- ^ Explicitly set the value of the input. Use 'never' if you
     -- don't need to do this.
     -> m (HtmlInput t, Dynamic t Text)
     -- ^ The input element and its current text value.
text attributes setValue = do
  (e, v) <- input' "text" attributes setValue
  pure (e, fromMaybe "" <$> v)
{-# INLINABLE text #-}

-- | An entry for a telephone number.
--
-- This behaves like a text entry with no additional structure or
-- validation because telephone numbers vary so much around the
-- world. However, browsers may change the onscreen keyboard or
-- display the element differently from plain text inputs.
tel :: forall m t. Dom t m
    => AttributeSet t "tel" "HTML"
    -- ^ Attributes
    -> Event t Text
    -- ^ Explicitly set the value of the input. Use 'never' if you
    -- don't need to do this.
    -> m (HtmlInput t, Dynamic t Text)
    -- ^ The input element and its current text value.
tel attributes setValue = do
  (e, v) <- input' "tel" attributes setValue
  pure (e, fromMaybe "" <$> v)
{-# INLINABLE tel #-}

-- | A plain text field used for searching.
--
-- This is functionally equivalent to a text field, but some browsers
-- present search elements differently (like including a clear button
-- or changing the enter key on the onscreen keyboard to a search
-- icon).
--
-- __Example__
--
-- Search input with a placeholder:
--
-- @
-- search [ placeholder =: "search" ] never
-- @
search :: forall m t. Dom t m
       => AttributeSet t "search" "HTML"
       -- ^ Attributes
       -> Event t Text
       -- ^ Explicitly set the text value of the input. Use 'never' if
       -- you don't need this.
       -> m (HtmlInput t, Dynamic t Text)
       -- ^ The input element and its current value.
search attributes setValue = do
  (e, v) <- input' "search" attributes setValue
  pure (e, fromMaybe "" <$> v)
{-# INLINABLE search #-}

-- | A plain text field used for URLs.
--
-- This works the same as a plain text input but has different
-- validation parameters and can be handled differently by browsers
-- with onscreen keyboards (ie mobile).
--
-- __Example__
--
-- URL entry with default value:
--
-- @
-- url [ url_value =: "https://example.com" ]
-- @
url :: forall m t. Dom t m
       => AttributeSet t "url" "HTML"
       -- ^ Attributes
       -> Event t Url
       -- ^ Explicitly set the text value of the input. Use 'never' if
       -- you don't need this.
       -> m (HtmlInput t, Dynamic t (Maybe Url))
       -- ^ The input element and its current value. 'Nothing' if the
       -- URL syntax is invalid.
url = input' "url"
{-# INLINABLE url #-}

-- | A password input.
--
-- On most browsers, the exact text the user types will be hidden,
-- with each character displayed as @*@ or @•@.
password :: forall m t. Dom t m
         => AttributeSet t "password" "HTML"
         -- ^ Attributes
         -> Event t Text
         -- ^ Override the current value. Use 'never' if you don't
         -- need this.
         -> m (HtmlInput t, Dynamic t Password)
         -- ^ The input element and the current value of the input.
password attributes setValue = do
  (e, v) <- input' "password" attributes setValue
  pure (e, Password . fromMaybe "" <$> v)
{-# INLINABLE password #-}

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
-- email [ email_value =: "john.doe@example.com" ] never
-- @
email :: forall m t. Dom t m
      => AttributeSet t "email" "HTML"
      -> Event t Email
      -> m (HtmlInput t, Dynamic t (Maybe Email))
email = input' "email"
{-# INLINABLE email #-}

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
-- email [ emails_value =: ["a@example.com", "b@example.com"] ] never
-- @
emails :: forall m t. (Dom t m)
       => AttributeSet t "emails" "HTML"
       -> Event t (Vector Email)
       -> m (HtmlInput t, Dynamic t (Vector (Maybe Email)))
emails attributes setEmails = do
  let config = def { setValue = Just $ toAttributeValue <$> setEmails }
  e <- input "email" attributes' config
  pure (e, getEmails <$> Dom.value e)
  where attributes' :: AttributeSet t "emails" "HTML"
        attributes' = attributes <>
          [ boolean @'["emails"] "multiple" =: True ]
        getEmails = Vector.fromList . map validate .
          filter (Text.all isHtmlWhitespace) . Text.split (== ',')
{-# INLINABLE emails #-}

-- ** Checkboxes and Radio Buttons

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
  let config = def { setChecked = Just setChecked  }
  HtmlInput e <- input "checkbox" attributes config
  pure (HtmlInput e, Dom._inputElement_checked e)
           -- TODO: does this always force the checkbox to start
           -- unchecked?
{-# INLINABLE checkbox #-}

-- ** Numbers

-- | An input that takes on numeric values: either @type="number"@ or
-- @type="range"@.
numeric :: forall type_ n m supports t.
           ( Num n
           , AsAttributeValue n
           , Dom t m
           , KnownSymbol type_
           , KnownSymbols supports
           , AttributeSet.Compatible type_ "HTML" supports
           )
        => Text
        -- ^ Type of input (usually @"number"@ or @"range"@)
        -> Attribute supports n
        -- ^ Specialized attribute for values of type @n@ (eg
        -- 'integer_value' or 'number_value').
        -> AttributeSet t type_ "HTML"
        -- ^ Attributes
        -> Event t n
        -- ^ Override current value. Use 'never' if you don't need
        -- this.
        -> m (HtmlInput t, Dynamic t n)
        -- ^ The input element and the current value of the input.
numeric type_ valueAttribute attributes setValue = do
  (e, v) <- input' type_ attributes setValue
  v' <- Reflex.improvingMaybe v
  pure case AttributeSet.lookup valueAttribute attributes of
    Just start -> (e, Reflex.zipDynWith fromMaybe start v')
    Nothing    -> (e, Reflex.zipDynWith fromMaybe (pure 0) v')
{-# INLINABLE numeric #-}

-- | An input that allows the user to specify any number.
--
-- Some browsers allow invalid characters in numeric input fields. If
-- this happens, the value will retain the previous valid number.
--
-- __Examples__
--
-- Numeric input starting at 12.5:
--
-- @
-- number [ number_value =: 12.5 ] never
-- @
--
-- Numeric input that only allows entries rounded to 0.5. The browser
-- should take care of the rounding:
--
-- @
-- number [ step =: 0.5 ] never
-- @
--
-- Restricting the range of the input:
--
-- @
-- number [ number_min =: 2.5, number_max =: 11, number_step =: Just 0.5 ] never
-- @
--
-- Restricting to integer values:
--
-- @
-- number [ number_step =: Just 1 ] never
-- @
--
-- This will still return the value as a 'Double'; you can use
-- 'integer' to get 'Integer' values instead.
number :: forall m t. Dom t m
       => AttributeSet t "number" "HTML"
       -> Event t Double
       -> m (HtmlInput t, Dynamic t Double)
number = numeric "number" number_value
{-# INLINABLE number #-}

-- | A number input restricted to integers.
--
-- Some browsers allow invalid characters in numeric input fields. If
-- this happens, the value will retain the previous valid number.
--
-- __Examples__
--
-- A numeric input ranging from 500 to 1000 with a step of 25:
--
-- @
-- integer [ integer_min =: 500, integer_max =: 1000, integer_step =: 25 ] never
-- @
integer :: forall m t. Dom t m
        => AttributeSet t "integer" "HTML"
        -> Event t Integer
        -> m (HtmlInput t, Dynamic t Integer)
integer = numeric "number" integer_value
{-# INLINABLE integer #-}

-- | A numeric input for numbers within a range.
--
-- This is typically represented by a slider or a spinner rather than
-- a text entry box, so it should not be used when the precise value
-- of the input matters.
--
-- __Examples__
--
-- The default behavior is ranging from 0 to 1 with no steps:
--
-- @
-- range [] never
--
-- -- equivalently:
-- range [ number_step =: Nothing, number_min =: 0, number_max =: 1 ] never
-- @
--
-- A range going from 0 to 1 with steps of 0.1:
--
-- @
-- range [ number_step =: Just 0.1 ] never
-- @
--
-- A range going from 0 to 100 with steps of 1:
--
-- @
-- range [ number_step =: Just 1, number_min =: 0, number_max =: 100 ] never
-- @
--
-- The result value is still returned as a 'Double'; if you only want
-- integers, consider 'integerRange' instead.
range :: forall m t. Dom t m
      => AttributeSet t "number" "HTML"
      -- ^ Attributes
      -> Event t Double
      -- ^ Set the number. Use 'never' if you don't need this.
      -> m (HtmlInput t, Dynamic t Double)
      -- ^ The element and the currently picked number
range attributes = numeric "range" number_value (base <> attributes)
  where base = [ number_min =: 0, number_max =: 1, number_step =: Nothing ]
{-# INLINABLE range #-}

-- | A numeric input for integers within a range.
--
-- Default behavior is to range from 0 to 100 with a step of 1.
--
-- See 'range' for more details.
--
-- __Examples__
--
-- Range from 500 to 1000 with a step of 25:
--
-- @
-- integerRange [ integer_min =: 500, integer_max =: 1000, integer_step =: 25 ] never
-- @
integerRange :: forall m t. Dom t m
             => AttributeSet t "integer" "HTML"
             -> Event t Integer
             -> m (HtmlInput t, Dynamic t Integer)
integerRange attributes = numeric "range" integer_value (base <> attributes)
  where base = [ integer_min =: 0, integer_max =: 100, integer_step =: 1 ]
{-# INLINABLE integerRange #-}

-- ** Date and Time

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
  (e, v) <- input' "month" attributes (toText <$> setMonth)
  pure (e, getMonth <$> v)
  where toText = Text.pack . Time.formatShow Time.yearMonthFormat
        getMonth v = v >>= Time.formatParseM Time.yearMonthFormat . Text.unpack
{-# INLINABLE month #-}

-- | A week + year input.
--
-- The underlying value should be in the @yyyy-Www@ format
-- (@2022-W37@).
--
--  __Example__
--
-- A week input with a default corresponding to @2022-W37@:
--
-- @
-- week [ week_value =: (2022, 37) ] never
-- @
week :: forall m t. Dom t m
     => AttributeSet t "week" "HTML"
     -- ^ Attributes
     -> Event t (Integer, Int)
     -- ^ Explicitly set the year-week value of the input. Use
     -- 'Reflex.never' if you don't need this.
     -> m (HtmlInput t, Dynamic t (Maybe (Integer, Int)))
     -- ^ The element and the current year-week pair if it parses.
week attributes setWeek = do
  (e, v) <- input' "week" attributes (toText <$> setWeek)
  pure (e, getMonth <$> v)
  where weekFormat = Time.yearWeekFormat Time.ExtendedFormat
        toText = Text.pack . Time.formatShow weekFormat
        getMonth v = v >>= Time.formatParseM weekFormat . Text.unpack
{-# INLINABLE week #-}

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
date = input' "date"
{-# INLINABLE date #-}

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
time = input' "time"
{-# INLINABLE time #-}

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
datetime = input' "datetime-local"
{-# INLINABLE datetime #-}

-- ** Misc

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
  (e, v) <- input' "color" attributes setColor
  pure (e, fromMaybe (Opaque Colour.black) <$> v)
{-# INLINABLE color #-}

-- * Input Attributes

-- | Whether an input element is enabled or disabled.
--
-- A disabled element should not accept user input and should have
-- some visual indication that it is disabled.
data Enabled = Enabled
             | Disabled
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | 'Enabled' if 'True', 'Disabled' if 'False'.
enabledIf :: Bool -> Enabled
enabledIf = bool Disabled Enabled

-- | Set whether an input is enabled or disabled.
enabled :: Attribute
  ('["button", "fieldset", "optgroup", "option", "select", "textarea"] <> Inputs)
  Enabled
enabled = logical "enabled" \case
  Enabled  -> []
  Disabled -> [("disabled", "")]

-- | Whether a checkbox /starts out checked when the page loads/.
--
-- Note: The @checked@ attribute /does not/ reflect or control the
-- state of the checkbox element after it's been loaded. Setting
-- @checked@ dynamically will not have a visible effect and querying
-- the attribute will not necessarily reflect the current state of the
-- checkbox.
--
-- __Examples__
--
-- -- @
-- checkbox [ checked =: True ] never
-- -- ⇒ <input type="checkbox" checked>
--
-- checkbox [ checked =: False ] never
-- -- ⇒ <input type="checkbox">
-- @
checked :: Attribute '["input", "checkbox"] Bool
checked = boolean "checked"

-- | The minimum number of a characters a password input accepts,
-- measured in UTF-16 code units.
minlength :: Attribute '["input", "password"] Word
minlength = native "minlength"

-- | The maximum number of a characters a password input accepts,
-- measured in UTF-16 code units.
maxlength :: Attribute '["input", "password"] Word
maxlength = native "maxlength"

-- ** Value

-- $ Different types of inputs have different types of values. To
-- support this with reasonable type inference and error messages, we
-- have versions of the @value@ attribute specialized to different
-- types.

-- | The default value element for inputs—unstructured text.
value :: Attribute '["input", "option", "text", "search", "tel"] Text
value = native "value"

-- | A value for numeric input fields.
number_value :: Attribute '["input", "number"] Double
number_value = native "value"

-- | The minimum number a numeric input should accept.
number_min :: Attribute '["input", "number"] Double
number_min = native "min"

-- | The maximum number a numeric input should accept.
number_max :: Attribute '["input", "number"] Double
number_max = native "max"

-- | The step by which a numeric input can change.
--
-- 'Nothing' gets translated to a value of @any@.
--
-- If the user enters a value that does not conform to the step, the
-- browser will round it to the nearest valid value.
number_step :: Attribute '["input", "number"] (Maybe Double)
number_step = logical "step" \case
  Just n  -> ["step" =. n]
  Nothing -> ["step" =. ("any" :: Text)]

-- | The value for numeric inputs restricted to integers.
integer_value :: Attribute '["input", "integer"] Integer
integer_value = native "value"

-- | The minimum value for numeric inputs restricted to integers.
integer_min :: Attribute '["input", "integer"] Integer
integer_min = native "min"

-- | The maximum value for numeric inputs restricted to integers.
integer_max :: Attribute '["input", "integer"] Integer
integer_max = native "max"

-- | The step by which an integer numeric input can change.
--
-- If the user enters a value that does not conform to the step, the
-- browser will round it to the nearest valid value.
integer_step :: Attribute '["input", "integer"] Integer
integer_step = native "step"

-- | A value for URL entries.
url_value :: Attribute '["input", "url"] Url
url_value = native "value"

-- | A value for colors. Only opaque colors are supported.
--
-- __Example__
--
-- @
-- color [ color_value =: Opaque Colour.red ] never
-- @
color_value :: Attribute '["input", "color"] Opaque
color_value = native "value"

-- | A value for month inputs: (year, day) pairs.
--
-- __Example__
--
-- @
-- month [ month_value =: (2022, 12) ] never
-- @
month_value :: Attribute '["month"] (Integer, Int)
month_value = logical "value" (\ month -> [("value", toText month)])
  where toText = Text.pack . Time.formatShow Time.yearMonthFormat

-- | A value for week inputs: (year, week) pairs.
--
-- __Example__
--
-- @
-- week [ week_value =: (2022, 37) ] never
-- @
week_value :: Attribute '["week"] (Integer, Int)
week_value = logical "value" (\ month -> [("value", toText month)])
  where toText = Text.pack . Time.formatShow (Time.yearWeekFormat Time.ExtendedFormat)

-- | A value for date pickers.
--
-- __Example__
--
-- @
-- date [ date_value =: read "2022-12-23" ] never
-- @
date_value :: Attribute '["date"] Day
date_value = native "value"

-- | A value for time pickers.
--
-- __Example__
--
-- @
-- time [ time_value =: read "14:25" ] never
-- @
time_value :: Attribute '["time"] TimeOfDay
time_value = native "value"

-- | A value for datetime-local inputs.
--
-- __Example__
--
-- @
-- datetime [ datetime_value =: read "2022-12-23 14:23:00" ] never
-- @
datetime_value :: Attribute '["datetime-local"] LocalTime
datetime_value = native "value"

-- | A value for email inputs.
--
-- __Example__
--
-- @
-- email [ email_value =: "john.doe@example.com" ]
-- @
email_value :: Attribute '["email"] Email
email_value = native "value"

-- | A value for email inputs allowing multiple emails.
--
-- __Example__
--
-- @
-- email [ emails_value =: ["a@example.com", "b@example.com"] ]
-- @
emails_value :: Attribute '["emails"] (Vector Email)
emails_value = native "value"
