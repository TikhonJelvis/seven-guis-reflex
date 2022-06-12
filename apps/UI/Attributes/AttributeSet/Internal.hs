{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The implementation of 'AttributeSet', exposing fiddly type-level
-- details that should /not/ leak to users of the public API.
--
-- The code in this module may change at any time.
module UI.Attributes.AttributeSet.Internal where

import           Data.Dependent.Map      (DMap)
import qualified Data.Dependent.Map      as DMap
import           Data.Functor            ((<&>))
import           Data.GADT.Compare       (GCompare (..), GEq (..),
                                          GOrdering (..))
import           Data.GADT.Show          (GShow (..))
import           Data.Kind               (Constraint, Type)
import           Data.Map                (Map)
import qualified Data.Map.Strict         as Map
import           Data.Some               (Some (..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..), display)
import qualified Data.Text.Lazy.Builder  as Builder
import           Data.Type.Bool
import           Data.Type.Equality

import           GHC.Exts                (IsList (..))
import           GHC.TypeLits            (ErrorMessage (..), Symbol, TypeError)

import           Reflex                  (Dynamic, Reflex, zipDynWith)

import qualified UI.Attributes.Attribute as Attribute
import           UI.Attributes.Attribute (AsAttributeValue (..), Attribute (..))
import           UI.Type.List            (Elem, KnownSymbols, ShowList)

import qualified Unsafe.Coerce           as Unsafe

-- * Attribute Set

-- | A set of attributes for some kind of element. Each attribute can
-- potentailly change over time.
--
-- Attribute sets track what kind of attribute is allowed based on the
-- element and its namespace (@"HTML"@, @"SVG"@... etc):
--
-- @
-- div :: Dom t m => AttributeSet t "div" "HTML" -> m a -> m (Html t, a)
-- @
--
-- Attributes can be defined as a list, using '=:' to provide static
-- values and '==:' to provide dynamic values:
--
-- @
-- myAttributes :: Attributes t ["HTML"]
-- myAttributes =
--   [ class_ =: ["draggable", "card"]
--   , position =: Relative
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
newtype AttributeSet t (element :: Symbol) (namespace :: Symbol) =
  AttributeSet (DMap (AttributeKey element namespace) (Dynamic t))

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
-- We can also combine static and dynamic values:
--
-- @
-- combine [class_ =: ["draggable"]]
--   [class_ ==: classIf "dragged" <$> beingDragged] ≡
--   [class_ ==: bool ["draggable"] ["dragged", "draggable"] <$> beingDragged]
-- @
combine :: forall (element :: Symbol) (namespace :: Symbol) t. Reflex t
        => AttributeSet t element namespace
        -- ^ Base values. May be overriden.
        -> AttributeSet t element namespace
        -- ^ Additional values. May override base values.
        -> AttributeSet t element namespace
combine (AttributeSet old) (AttributeSet new) = AttributeSet $
  DMap.unionWithKey combineAttribute old new
  where combineAttribute :: AttributeKey element namespace v
                         -> Dynamic t v
                         -> Dynamic t v
                         -> Dynamic t v
        combineAttribute (AttributeKey _) =
          Reflex.zipDynWith combineAttributeValues

-- | '<>' is 'combine'
instance Reflex t => Semigroup (AttributeSet t element namespace) where
  (<>) = combine

-- | 'mempty' is 'emtpy'
instance Reflex t => Monoid (AttributeSet t element namespace) where
  mempty = empty

-- | An empty set of attributes.
empty :: AttributeSet t element namespace
empty = AttributeSet DMap.empty

-- | A set of attributes with a single attribute set.
--
-- >>> singleton (class_ =: ["draggable"])
singleton :: forall (element :: Symbol) (namespace :: Symbol) t. Reflex t
          => SetAttribute t element namespace
          -> AttributeSet t element namespace
singleton = \case
  SetAttribute attribute value ->
    AttributeSet $ DMap.singleton (AttributeKey attribute) value

instance Display (AttributeSet t element namespace) where
  displayBuilder (AttributeSet dmap) =
    "[" <> Builder.fromText (Text.intercalate " " displayed) <> "]"
    where displayed = DMap.keys dmap <&> \case
            Some key -> display key <> "=\"...\""

instance Reflex t => IsList (AttributeSet t element namespace) where
  type Item (AttributeSet t element namespace) = SetAttribute t element namespace

  fromList :: [SetAttribute t element namespace] -> AttributeSet t element namespace
  fromList sets = AttributeSet $ foldr go DMap.empty sets
    where go :: SetAttribute t element namespace
             -> DMap (AttributeKey element namespace) (Dynamic t)
             -> DMap (AttributeKey element namespace) (Dynamic t)
          go (SetAttribute attribute v)=
            DMap.insertWithKey' combineDyn (AttributeKey attribute) v

          combineDyn _ = Reflex.zipDynWith combineAttributeValues

  toList :: AttributeSet t element namespace -> [SetAttribute t element namespace]
  toList (AttributeSet dmap) = DMap.foldrWithKey go [] dmap
    where go :: forall v. AttributeKey element namespace v
             -> Dynamic t v
             -> [SetAttribute t element namespace]
             -> [SetAttribute t element namespace]
          go (AttributeKey attribute) v sets = SetAttribute attribute v : sets

-- * AttributeKey

-- | The type used internally in 'AttributeSet' to track the type of
-- each value corresponding to an attribute in the set
-- (@AttributeValue name supports@).
data AttributeKey element namespace (a :: Type) where
  AttributeKey :: ( KnownSymbols supports
                  , Compatible element namespace supports
                  , AsAttributeValue a)
               => Attribute supports a
               -> AttributeKey element namespace a

instance Show (AttributeKey element namespace a) where
  show (AttributeKey attribute) = show attribute

instance Display (AttributeKey element namespace a) where
  displayBuilder (AttributeKey attribute) = displayBuilder attribute

instance GShow (AttributeKey element namespace) where gshowsPrec = showsPrec

instance GEq (AttributeKey element namespace) where
  geq (AttributeKey a) (AttributeKey b)
    | Attribute.name a == Attribute.name b &&
      Attribute.supports a == Attribute.supports b =
      Just $ Unsafe.unsafeCoerce Refl
      -- this is safe as long as we don't define and use an attribute
      -- with the same name and supported elements but different types
      --
      -- I had an earlier design of the code that kept track of
      -- attribute names at the type level and prevented this
      -- possibility, but the extra complexity did not seem worth it

    | otherwise = Nothing

instance GCompare (AttributeKey element namespace) where
  gcompare ka@(AttributeKey a) kb@(AttributeKey b)
    | Just Refl <- geq ka kb            = GEQ
    | Attribute.name a < Attribute.name b = GLT
    | otherwise                         = GGT

-- * Element compatibility

-- $ Some type-level functions to determine whether an attribute is
-- compatible with a specific attribute set.
--
-- Each element will have its /accepted/ set in its type:
--
-- @
-- a :: forall a m t. (Reflex t, Dom t m)
--   => AttributeSet t '["a", "HTML"] -> m a -> m (Html t, a)
-- @
--
-- Each attribute tracks which elements or namespaces it supports:
--
-- @
-- class_ :: Attribute "class" ["HTML", "SVG"] (Set ClassName)
-- href :: Attribute "href" ["a", "area", "base", "link"] URL
-- @
--
-- An attribute can be included in an attribute set as long as /any/
-- of its supported types match /any/ of the set's compatible
-- types. An @a@ element would be able to take both @class_@ and
-- @href@, while a @div@ would be able to take @class_@ but /not/
-- @href@.
--
-- To accomplish this we need to do a bit of type-level computation to
-- match things up.

type family Compatible element namespace supports :: Constraint where
  Compatible element namespace supports =
    If (Elem element supports || Elem namespace supports)
       (() :: Constraint)
       (Incompatible element namespace supports)

type family Incompatible element namespace supports :: Constraint where
  Incompatible element namespace supports = TypeError
    ('Text "Incompatible attribute" ':$$:
     'Text "Element: " ':<>: 'Text element ':<>:
     'Text "(" ':<>: 'Text namespace ':<>: 'Text ")" ':$$:
     'Text "Attribute supports: " ':<>: 'Text (ShowList supports))

-- * SetAttribute

-- | Set an attribute to a value of a compatible type.
data SetAttribute t element namespace where
  SetAttribute :: ( KnownSymbols supports
                  , AsAttributeValue a
                  , Compatible element namespace supports
                  )
               => Attribute supports a
               -> Dynamic t a
               -> SetAttribute t element namespace

-- ** Rendering to reflex-dom

-- | Convert an 'AttributeSet' to an untyped map of elements in the
-- format expected by @reflex-dom@ functions.
--
-- This is morally equivalent to:
--
-- @
-- Map.fromList [ (Attribute.name attribute, toAttributeValue value)
--              | (attribute, value) <- toList attributeSet
--              ]
-- @
toDom :: forall t element namespace. Reflex t
      => AttributeSet t element namespace
      -> Dynamic t (Map Text Text)
toDom (AttributeSet dmap) = DMap.foldrWithKey go (pure []) dmap
  where go :: forall v. AttributeKey element namespace v
           -> Dynamic t v
           -> Dynamic t (Map Text Text)
           -> Dynamic t (Map Text Text)
        go (AttributeKey (Attribute.name -> name)) value =
          Reflex.zipDynWith (<>) $ convert name <$> value

        convert :: forall a. AsAttributeValue a => Text -> a -> Map Text Text
        convert name = Map.singleton name . toAttributeValue
