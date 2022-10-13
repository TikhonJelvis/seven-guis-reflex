-- | Global attributes that apply to both HTML and SVG elements.
module UI.Attributes
  ( module UI.Attributes.Attribute

  , class_
  , id_
  , style

  , href
  )
where

import qualified Data.Map                as Map
import           Data.Set                (Set)

import           UI.Attributes.Attribute
import           UI.Class                (ClassName)
import qualified UI.Css.Rules            as Css
import           UI.Css.Rules            (CssRules)
import           UI.Id                   (Id)
import           UI.Url                  (Url)

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
id_ :: Attribute Id
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
class_ :: Attribute (Set ClassName)
class_ = native "class"

-- | Set the CSS styles for an element.
style :: Attribute CssRules
style = logical "style" \ existing new ->
  let go _ = Css.withRules \case
        Just old -> old <> new
        Nothing  -> new
  in Map.insertWith go "style" (toAttributeValue new) existing

-- * Element Attributes

-- | The URL of a linked resource.
--
-- __Example__
--
-- @
-- a [href =: "https://example.com"] "link to example.com"
-- @
href :: Attribute Url
href = native "href"
