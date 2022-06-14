-- | Attributes that only apply to HTML elements.
module UI.Html.Attributes where

import           Data.Bool     (bool)
import           Data.Hashable (Hashable)
import           Data.Text     (Text)

import           GHC.Generics  (Generic)

import           UI.Attributes (Attribute, logical, native)
import           UI.Id         (Id, Ids)
import           UI.Url        (Url)

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
