{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The implementation of 'AttributeSet', exposing fiddly type-level
-- details that should /not/ leak to users of the public API.
--
-- The code in this module may change at any time.
module UI.Attributes.AttributeSet.Internal where

import           Control.Applicative     (liftA2)

import           Data.Dependent.Map      (DMap)
import qualified Data.Dependent.Map      as DMap
import           Data.Functor            ((<&>))
import           Data.Map                (Map)
import           Data.Some               (Some (..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..), display)
import qualified Data.Text.Lazy.Builder  as Builder

import           GHC.Exts                (IsList (..))
import           GHC.Generics            (Generic)

import qualified UI.Attributes.Attribute as Attribute
import           UI.Attributes.Attribute (Attribute)


-- * Attribute Set

-- | A set of attributes for some kind of element. Each attribute is
-- wrapped in a functor, supporting things like using 'Dynamic' to let
-- attributes vary over time.
--
-- Attributes can be defined as a list, using '=:' to provide pure
-- values and '==:' to provide values in the functor:
--
-- @
-- myAttributes :: Attributes t
-- myAttributes =
--   [ class_ =: ["draggable", "card"]
--   , id =: "my-card"
--   ]
-- @
--
-- The same attribute can be specified multiple times. For most
-- attributes, this will only keep the /last/ value specified, but
-- some attributes like 'class_' and 'transform' will have multiple
-- values collected into one.
--
-- For example, we can set a few classes statically and an additional
-- class dynamically. The resulting element will have the classes
-- @card draggable dragged@ when it is dragged and @card draggable@
-- when it isn't:
--
-- @
-- div [ class_ =: ["draggable", "card"]
--     , class_ ==: classIf "dragged" <$> beingDragged
--     ]
-- @
newtype AttributeSet f = AttributeSet (DMap Attribute f)
  deriving stock (Generic)

-- | Combine attributes. Attributes set in both sets are combined with
-- 'combineAttributeValues'.
--
-- Semantically equivalent to @Map.unionWith combineAttributeValues@.
--
-- __Examples__
--
-- Most attributes take the value from the /second/ argument:
--
-- @
-- combine [id_ =: "abc"] [id_ =: "def"] ≡ [id_ =: "def"]
-- @
--
-- Some attributes combine values from /both/ arguments:
--
-- @
-- combine [class_ =: ["draggable"]] [class_ =: ["card"]] ≡
--   [class_ =: ["draggable", "card"]]
-- @
--
-- We can also combine pure and wrapped values:
--
-- @
-- combine [class_ =: ["draggable"]]
--   [class_ ==: classIf "dragged" <$> beingDragged] ≡
--   [class_ ==: bool ["draggable"] ["dragged", "draggable"] <$> beingDragged]
-- @
combine :: forall f. Applicative f
        => AttributeSet f
        -- ^ Base values. May be overriden.
        -> AttributeSet f
        -- ^ Additional values. May override base values.
        -> AttributeSet f
combine (AttributeSet old) (AttributeSet new) = AttributeSet $
  DMap.unionWithKey combineAttribute old new
  where combineAttribute :: Attribute v -> f v -> f v -> f v
        combineAttribute attribute = liftA2 $ Attribute.combine attribute

-- | '<>' is 'combine'
instance Applicative f => Semigroup (AttributeSet f) where
  (<>) = combine

-- | 'mempty' is 'emtpy'
instance Applicative f => Monoid (AttributeSet f) where
  mempty = empty

-- | An empty set of attributes.
--
-- >>> display empty
-- "[]"
empty :: forall f. AttributeSet f
empty = AttributeSet DMap.empty

-- | A set of attributes with a single attribute set.
singleton :: forall f. Applicative f => SetAttribute f -> AttributeSet f
singleton = \case
  SetAttribute attribute value -> AttributeSet $ DMap.singleton attribute value

instance Display (AttributeSet f) where
  displayBuilder (AttributeSet dmap) =
    "[" <> Builder.fromText (Text.intercalate " " displayed) <> "]"
    where displayed = DMap.keys dmap <&> \case
            Some key -> display key <> "=\"...\""

instance Applicative f => IsList (AttributeSet f) where
  type Item (AttributeSet f) = SetAttribute f

  fromList :: [SetAttribute f] -> AttributeSet f
  fromList sets = AttributeSet $ foldr go DMap.empty sets
    where go :: SetAttribute f
             -> DMap Attribute f
             -> DMap Attribute f
          go (SetAttribute attribute v) =
            DMap.insertWithKey' (const $ liftA2 $ Attribute.combine attribute) attribute v

  toList :: AttributeSet f -> [SetAttribute f]
  toList (AttributeSet dmap) = DMap.foldrWithKey go [] dmap
    where go :: forall v. Attribute v
             -> f v
             -> [SetAttribute f]
             -> [SetAttribute f]
          go attribute v sets = SetAttribute attribute v : sets

-- * Accessing Attribute Values

-- | Look up the value set for the given attribute, if any.
lookup :: forall a f. Attribute a -> AttributeSet f -> Maybe (f a)
lookup attribute (AttributeSet dmap) = DMap.lookup attribute dmap

-- * Setting Attributes

-- | Set or update an attribute.
data SetAttribute f where
  SetAttribute :: Attribute a -> f a -> SetAttribute f

-- | Set an attribute to a static value that does /not/ change over
-- time.
--
-- __Example__
--
-- @
-- div [ class_ =: ["draggable", "card"] ]
-- @
(=:) :: forall a f. (Applicative f)
     => Attribute a
     -- ^ attribute
     -> a
     -- ^ static value
     -> SetAttribute f
attribute =: value = SetAttribute attribute (pure value)
infixr 1 =:

-- | Set an attribute to a dynamic value that /can/ change over time.
--
-- __Example__
--
-- @
-- div [ class_ ==: classIf "dragged" <$> beingDragged ]
-- @
(==:) :: forall a f. Attribute a
      -- ^ attribute
      -> f a
      -- ^ dynamic value
      -> SetAttribute f
attribute ==: value = SetAttribute attribute value
infixr 1 ==:

-- * Rendering to text

-- | Convert an 'AttributeSet' to text map of attribute-value pairs.
--
-- This is morally equivalent to:
--
-- @
-- Map.fromList [ (Attribute.name attribute, toAttributeValue value)
--              | (attribute, value) <- toList attributeSet
--              ]
-- @
toDom :: forall f. Applicative f => AttributeSet f -> f (Map Text Text)
toDom (AttributeSet dmap) = DMap.foldlWithKey go (pure []) dmap
  where go :: forall v. f (Map Text Text) -> Attribute v -> f v -> f (Map Text Text)
        go existing attribute value =
          let newAttributes = Attribute.toAttributes attribute <$> existing <*> value
          in liftA2 (<>) newAttributes existing
