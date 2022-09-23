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

    -- ** Inspecting attribute sets
  , lookup

    -- ** Integrating with reflex-dom
  , toDom
  )
where

import           Prelude                             hiding (lookup)

import           UI.Attributes.AttributeSet.Internal
