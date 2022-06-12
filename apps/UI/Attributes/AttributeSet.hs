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


import           GHC.TypeLits                        (KnownSymbol)

import           Reflex                              (Dynamic)

import           UI.Attributes.Attribute             (AsAttributeValue (..),
                                                      Attribute (..),
                                                      AttributeValue)
import           UI.Attributes.AttributeSet.Internal
import           UI.Type.List                        (KnownSymbols)


-- TODO: consider implement non-Reflex static version of Attributes

-- | Set an attribute to a static value that does /not/ change over
-- time.
--
-- __Example__
--
-- @
-- div [ class_ := ["draggable", "card"] ]
-- @
(=:) :: ( KnownSymbol name
        , KnownSymbols supports
        , AsAttributeValue (AttributeValue name supports)
        , Compatible element namespace name supports
        )
     => Attribute name supports
     -- ^ attribute
     -> AttributeValue name supports
     -- ^ static value
     -> SetAttribute t element namespace
(=:) = Constant
infixr 1 =:

-- | Set an attribute to a dynamic value that /can/ change over time.
--
-- __Example__
--
-- @
-- div [ class_ :== classIf "dragged" <$> beingDragged ]
-- @
(==:) :: ( KnownSymbol name
         , KnownSymbols supports
         , AsAttributeValue (AttributeValue name supports)
         , Compatible element namespace name supports
         )
      => Attribute name supports
      -- ^ attribute
      -> Dynamic t (AttributeValue name supports)
      -- ^ dynamic value
      -> SetAttribute t element namespace
(==:) = Dynamic
infixr 1 ==:
