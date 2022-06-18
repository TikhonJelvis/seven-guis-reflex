-- | Attributes that only apply to HTML elements.
module UI.Html.Attributes where

import           Data.Text     (Text)

import           UI.Attributes (Attribute, native)
import           UI.Url        (Url)

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
