-- | Elements can take different kinds of attributes with different
-- types of values.
--
-- 'AttributeSet' is a type that maps attributes to their
-- corresponding type of value, with everything being able to change
-- over time ('Reflex.Dynamic').
module UI.Attributes.AttributeSet
  ( -- * Attribute sets
    AttributeSet

    -- ** Setting attributes
  , SetAttribute (..)
  , (=:)
  , (==:)

    -- ** Constructing attribute sets
  , singleton
  , empty
  , combine

    -- ** Integrating with reflex-dom
  , toDom
  )
where

import           Reflex                              (Dynamic, Reflex)

import           UI.Attributes.Attribute             (AsAttributeValue (..),
                                                      Attribute (..))
import           UI.Attributes.AttributeSet.Internal
import           UI.Type.List                        (KnownSymbols)


-- TODO: consider implement non-Reflex static version of Attributes

-- | Set an attribute to a static value that does /not/ change over
-- time.
--
-- __Example__
--
-- @
-- div [ class_ =: ["draggable", "card"] ]
-- @
(=:) :: ( Reflex t
        , KnownSymbols supports
        , AsAttributeValue a
        , Compatible element namespace supports
        )
     => Attribute supports a
     -- ^ attribute
     -> a
     -- ^ static value
     -> SetAttribute t element namespace
attribute =: value = SetAttribute attribute (pure value)
infixr 1 =:

-- | Set an attribute to a dynamic value that /can/ change over time.
--
-- __Example__
--
-- @
-- div [ class_ ==: classIf "dragged" <$> beingDragged ]
-- @
(==:) :: ( KnownSymbols supports
         , AsAttributeValue a
         , Compatible element namespace supports
         )
      => Attribute supports a
      -- ^ attribute
      -> Dynamic t a
      -- ^ dynamic value
      -> SetAttribute t element namespace
(==:) = SetAttribute
infixr 1 ==:
