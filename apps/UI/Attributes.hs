-- | Global attributes that apply to both HTML and SVG elements.
module UI.Attributes
  ( module UI.Attributes.Attribute
  , module UI.Attributes.AttributeSet

  , class_
  , id_
  , style

  , href

  , ToCss
  , Length
  , px
  , RelativeLength
  , Factor
  , Angle (..)
  , Duration (..)
  , ms
  , s
  , styles
  , joinStyles
  , setProperty
  , updateProperty
  , setUserSelect

  , Transform (..)
  , addTransform
  , setTransform
  , translate
  , rotate
  , scale

  , Transition (..)
  , transition
  )
where

import           Data.Set                   (Set)

import           UI.Attributes.Attribute
import           UI.Attributes.AttributeSet
import           UI.Class                   (ClassName)
import           UI.Css                     (CssRules)
import           UI.Id                      (Id)
import           UI.Style
import           UI.Url                     (Url)

-- * Global Attributes

-- ** Ids

-- | The @id@ attribute of an element. Should be unique across the
-- entire document.
--
-- __Example__
--
-- @
-- div_ [ id_ =: "special-div" ]
-- @
--
-- would be matched by the CSS rule:
--
-- @
-- #special-div {
--   ...
-- }
-- @
id_ :: Attribute ["HTML", "SVG"] Id
id_ = native "id"

-- ** CSS Classes

-- | An element's CSS classes.
--
-- When set multiple times, all the CSS classes being set are combined
-- into one 'Set'.
--
-- __Examples__
--
-- @
-- div_ [class_ =: ["draggable"]]
-- @
--
-- would be matched by the CSS rule:
--
-- @
-- .draggable {
--   ...
-- }
-- @
--
-- Setting two classes statically and another class dynamically:
--
-- @
-- div_ [ class_ =: ["draggable", "card"]
--      , class_ ==: classIf "dragged" <$> beingDragged
--      ]
-- @
class_ :: Attribute ["HTML", "SVG"] (Set ClassName)
class_ = native "class"

         -- TODO: structured version of CSS style attribute
-- | Set the CSS styles for an element.
style :: Attribute ["HTML", "SVG"] CssRules
style = native "style"

-- * Element Attributes

-- | The URL of a linked resource.
--
-- __Example__
--
-- @
-- a [href =: "https://example.com"] "link to example.com"
-- @
href :: Attribute ["a", "area", "base", "link", "use"] Url
href = native "href"

