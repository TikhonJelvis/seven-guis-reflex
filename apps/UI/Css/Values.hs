-- | Values for CSS properties generally use the same lexical syntax
-- as HTML/SVG attribute values, except CSS properties can also always
-- be one of several __global values__:
--
--  * @inherit@
--  * @initial@
--  * @revert@
--  * @revert-layer@
--  * @unset@
--
-- This is expressed in Haskell through the 'Css' type and the
-- 'AsAttributeValue' class.
module UI.Css.Values
  ( Css (..)
  , module UI.Units
  )
where

import           Data.Hashable         (Hashable)
import           Data.Vector.Instances ()

import           GHC.Generics          (Generic)

import           UI.Attributes         (AsAttributeValue (..),
                                        CombineAttributeValue (combineAttributeValues))
import           UI.Units

-- * CSS Values

-- | A CSS value of the given type.
--
-- This can either be a value of the underlying type /or/ one of CSS's
-- __global values__:
--
--  * @inherit@
--  * @initial@
--  * @revert@
--  * @revert-layer@
--  * @unset@
data Css a = Value a
           | Inherit
           | Initial
           | Revert
           | RevertLayer
           | Unset
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance CombineAttributeValue a => CombineAttributeValue (Css a) where
  combineAttributeValues a b
    | isSpecial a || isSpecial b = b
    | otherwise                  = combineAttributeValues a b
    where isSpecial Value{} = False
          isSpecial _       = True

instance AsAttributeValue a => AsAttributeValue (Css a) where
  toAttributeValue = \case
    Value a     -> toAttributeValue a
    Inherit     -> "inherit"
    Initial     -> "initial"
    Revert      -> "revert"
    RevertLayer -> "revert-layer"
    Unset       -> "unset"

  fromAttributeValue = \case
    "inherit"      -> Just Inherit
    "initial"      -> Just Initial
    "revert"       -> Just Revert
    "revert-layer" -> Just RevertLayer
    "unset"        -> Just Unset
    other          -> Value <$> fromAttributeValue other
