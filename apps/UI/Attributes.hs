{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module UI.Attributes
  ( module UI.Attributes.Attribute
  , module UI.Attributes.AttributeSet

  , class_
  , id_
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
-- div [ id_ := "special-div" ]
-- @
--
-- would be matched by the CSS rule:
--
-- @
-- #special-div {
--   ...
-- }
-- @
id_ :: Attribute "id" ["HTML", "SVG"]
id_ = Attribute

type instance AttributeValue "id" ["HTML", "SVG"] = Id

-- ** CSS Classes

-- | An element's CSS classes.
--
-- When set multiple times, all the CSS classes being set are combined
-- into one 'Set'.
--
-- __Examples__
--
-- @
-- div [class_ := ["draggable"]]
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
-- div [ class_ := ["draggable", "card"]
--     , class_ :== classIf "dragged" <$> beingDragged
--     ]
-- @
class_ :: Attribute "class" ["HTML", "SVG"]
class_ = Attribute

type instance AttributeValue "class" ["HTML", "SVG"] = Set ClassName

-- * Element Attributes

-- | The URL of a linked resource.
--
-- __Example__
--
-- @
-- a [href := "https://example.com"] "link to example.com"
-- @
href :: Attribute "href" ["a", "area", "base", "link", "use"]
href = Attribute

type instance AttributeValue "href" ["a", "area", "base", "link", "use"] = Url
