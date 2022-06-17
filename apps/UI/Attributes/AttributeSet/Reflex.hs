-- | A version of the 'AttributeSet' API specialized to 'Dynamic'.
module UI.Attributes.AttributeSet.Reflex
  ( -- * Attribute sets
    AttributeSet

    -- ** Setting attributes
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

import           Reflex                              (Dynamic)

import qualified UI.Attributes.AttributeSet.Internal as Internal
import           UI.Attributes.AttributeSet.Internal hiding (AttributeSet)

-- | 'Internal.AttributeSet' specialized to 'Dynamic'
type AttributeSet t element namespace =
  Internal.AttributeSet (Dynamic t) element namespace

