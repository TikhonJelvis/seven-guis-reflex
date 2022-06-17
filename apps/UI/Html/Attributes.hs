-- | Attributes that only apply to HTML elements.
module UI.Html.Attributes where

import           Data.Bool                (bool)
import           Data.Hashable            (Hashable)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Time                (Day, LocalTime, TimeOfDay)
import qualified Data.Time.Format.ISO8601 as Time
import           Data.Vector              (Vector)

import           GHC.Generics             (Generic)

import           UI.Attributes            (Attribute, boolean, logical, native,
                                           (=.))
import           UI.Color                 (Opaque)
import           UI.Email                 (Email)
import           UI.Id                    (Id, Ids)
import           UI.Url                   (Url)

-- * Images and Media

-- | An alternate text description for the image. The description will
-- be presented if the image cannot be loaded or if the user is not
-- loading images (eg using a text-only browser or accessibility tool).
--
-- An @img@ without an @alt@ attribute represents an image that is a
-- key part of the content but has /no/ text equivalent. To indicate
-- an image is /not/ a key part of the content—it is purely decorative
-- or otherwise has no semantic content—set @alt@ to @""@.
--
-- __Examples__
--
-- A normal alt text:
--
-- @
-- img [ src =: "img/example.png", alt =: "An example image." ]
-- @
--
-- A purely decorative image with no semantic content:
--
-- @
-- img [ src =: "img/decoration.png", alt =: "" ]
-- @
alt :: Attribute '["img"] Text
alt = native "alt"

-- | The URL of the resource to fetch and embed in the document.
--
-- __Example__
--
-- @
-- img [ src =: "img/example.png", alt =: "An example image." ]
-- @
src :: Attribute '["img"] Url
src = native "src"

-- * Forms and Inputs

-- | Associates a 'label' or 'output' with a control.
--
-- 'input' elements can only be associated with one element, but
-- 'output' elements can support multiple elements. To set multiple
-- elements for an 'output', see 'fors'.
--
-- __Example__
--
-- Associate a label with a text input. When activated (eg by
-- clicking), the label will move the focus to the associated input.
--
-- @
-- example = do
--   input [ type_ =: Text, id_ =: "username" ]
--   label [ for =: "username" ] (text "username")
-- @
for :: Attribute '["label", "output"] Id
for = native "for"

-- | Associates an 'output' with /multiple/ elements. Plural version
-- of 'for'.
--
-- Used multiple times, this attribute will combine the values into a
-- single list.
--
-- __Example__
--
-- Associate an 'output' with two inputs:
--
-- @
-- example = do
--   input [ type_ =: Range, id_ =: "a", value =: "5" ]
--   input [ type_ =: Range, id_ =: "b", value =: "10" ]
--   output [ for_ =: ["a", "b"] ] (text "15")
-- @
--
-- Alternative, equivalent option:
--
-- @
-- example = do
--   input [ type_ =: Range, id_ =: "a", value =: "5" ]
--   input [ type_ =: Range, id_ =: "b", value =: "10" ]
--   output [ for_ =: ["a"], for_ =: ["b"] ] (text "15")
-- @
for_ :: Attribute '["output"] Ids
for_ = native "for"

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
  '["button", "input", "fieldset", "optgroup", "option", "select", "textarea"]
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

-- ** Input Value

-- $ Different types of inputs have different types of values. To
-- support this with reasonable type inference and error messages, we
-- have versions of the @value@ attribute specialized to different
-- types.

-- | The default value element for inputs—unstructured text.
value :: Attribute '["input", "text", "search", "tel"] Text
value = native "value"

-- | A value for numeric input fields.
number_value :: Attribute '["input", "number", "range"] Double
number_value = native "value"

-- | The minimum number a numeric input should accept.
number_min :: Attribute '["input", "number", "range"] Double
number_min = native "min"

-- | The maximum number a numeric input should accept.
number_max :: Attribute '["input", "number", "range"] Double
number_max = native "max"

-- | The step by which a numeric input can change.
--
-- 'Nothing' gets translated to a value of @any@.
--
-- If the user enters a value that does not conform to the step, the
-- browser will round it to the nearest valid value.
number_step :: Attribute '["input", "number", "range"] (Maybe Double)
number_step = logical "step" \case
  Just n  -> ["step" =. n]
  Nothing -> ["step" =. ("any" :: Text)]

-- | The value for numeric inputs restricted to integers.
integer_value :: Attribute '["input", "integer", "integer-range"] Integer
integer_value = native "value"

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
