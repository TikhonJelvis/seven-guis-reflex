{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeOperators        #-}
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
import           Data.GADT.Compare       (GCompare (..), GEq (..),
                                          GOrdering (..))
import           Data.GADT.Show          (GShow (..))
import           Data.Kind               (Constraint, Type)
import           Data.Map                (Map)
import           Data.Some               (Some (..))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display (..), display)
import qualified Data.Text.Lazy.Builder  as Builder
import           Data.Type.Bool
import           Data.Type.Equality

import           GHC.Exts                (IsList (..))
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (ErrorMessage (..), Symbol, TypeError)

import qualified UI.Attributes.Attribute as Attribute
import           UI.Attributes.Attribute (Attribute)
import           UI.Type.List            (Elem, KnownSymbols, ShowList)

import qualified Unsafe.Coerce           as Unsafe

-- * Attribute Set

-- | A set of attributes for some kind of element. Each attribute is
-- wrapped in a functor, supporting things like using 'Dynamic' to let
-- attributes vary over time.
--
-- Attribute sets track what kind of attribute is allowed based on the
-- element and its namespace (@"HTML"@, @"SVG"@... etc):
--
-- @
-- div :: Dom t m => AttributeSet t "div" "HTML" -> m a -> m (Html t, a)
-- @
--
-- Attributes can be defined as a list, using '=:' to provide pure
-- values and '==:' to provide values in the functor:
--
-- @
-- myAttributes :: Attributes t ["HTML"]
-- myAttributes =
--   [ class_ =: ["draggable", "card"]
--   , position =: Relative
--   , rotate ==: Turn <$> rotateControl
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
newtype AttributeSet f (element :: Symbol) (namespace :: Symbol) =
  AttributeSet (DMap (AttributeKey element namespace) f)
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
combine :: forall (element :: Symbol) (namespace :: Symbol) f. Applicative f
        => AttributeSet f element namespace
        -- ^ Base values. May be overriden.
        -> AttributeSet f element namespace
        -- ^ Additional values. May override base values.
        -> AttributeSet f element namespace
combine (AttributeSet old) (AttributeSet new) = AttributeSet $
  DMap.unionWithKey combineAttribute old new
  where combineAttribute :: AttributeKey element namespace v -> f v -> f v -> f v
        combineAttribute (AttributeKey attribute) =
          liftA2 (Attribute.combine attribute)

-- | '<>' is 'combine'
instance Applicative f => Semigroup (AttributeSet f element namespace) where
  (<>) = combine

-- | 'mempty' is 'emtpy'
instance Applicative f => Monoid (AttributeSet f element namespace) where
  mempty = empty

-- | An empty set of attributes.
empty :: forall f element namespace. AttributeSet f element namespace
empty = AttributeSet DMap.empty

-- | A set of attributes with a single attribute set.
--
-- >>> import UI.Attributes.Attribute (override)
-- >>> import Data.Functor.Identity (Identity)
-- >>> display $ singleton @Identity @"HTML" (override @'["HTML"] "foo" =: "bar")
-- "[foo=\"...\"]"
singleton :: forall f (element :: Symbol) (namespace :: Symbol). Applicative f
          => SetAttribute f element namespace
          -> AttributeSet f element namespace
singleton = \case
  SetAttribute attribute value ->
    AttributeSet $ DMap.singleton (AttributeKey attribute) value

instance Display (AttributeSet f element namespace) where
  displayBuilder (AttributeSet dmap) =
    "[" <> Builder.fromText (Text.intercalate " " displayed) <> "]"
    where displayed = DMap.keys dmap <&> \case
            Some key -> display key <> "=\"...\""

instance Applicative f => IsList (AttributeSet f element namespace) where
  type Item (AttributeSet f element namespace) = SetAttribute f element namespace

  fromList :: [SetAttribute f element namespace] -> AttributeSet f element namespace
  fromList sets = AttributeSet $ foldr go DMap.empty sets
    where go :: SetAttribute f element namespace
             -> DMap (AttributeKey element namespace) f
             -> DMap (AttributeKey element namespace) f
          go (SetAttribute attribute v)=
            DMap.insertWithKey' (combineDyn attribute) (AttributeKey attribute) v

          combineDyn attribute _ = liftA2 (Attribute.combine attribute)

  toList :: AttributeSet f element namespace -> [SetAttribute f element namespace]
  toList (AttributeSet dmap) = DMap.foldrWithKey go [] dmap
    where go :: forall v. AttributeKey element namespace v
             -> f v
             -> [SetAttribute f element namespace]
             -> [SetAttribute f element namespace]
          go (AttributeKey attribute) v sets = SetAttribute attribute v : sets

-- * Accessing Attribute Values

-- | Look up the value set for the given attribute, if any.
lookup :: forall a supports element namespace f.
          (KnownSymbols supports, Compatible element namespace supports)
       => Attribute supports a
       -> AttributeSet f element namespace
       -> Maybe (f a)
lookup attribute (AttributeSet dmap) =
  DMap.lookup (AttributeKey attribute) dmap

-- * AttributeKey

-- | The type used internally in 'AttributeSet' to track the type of
-- the value that corresponds to an attribute.
data AttributeKey element namespace (a :: Type) where
  AttributeKey :: (KnownSymbols supports, Compatible element namespace supports)
               => Attribute supports a
               -> AttributeKey element namespace a

instance Show (AttributeKey element namespace a) where
  show (AttributeKey attribute) =
    "AttributeKey " <> show (Attribute.name attribute) <>
    " (" <> show (Attribute.type_ attribute) <> ")"

instance Display (AttributeKey element namespace a) where
  displayBuilder (AttributeKey attribute) = displayBuilder attribute

instance GShow (AttributeKey element namespace) where gshowsPrec = showsPrec

instance GEq (AttributeKey element namespace) where
  geq (AttributeKey a) (AttributeKey b)
    | Attribute.type_ a == Attribute.type_ b &&
      Attribute.name a  == Attribute.name b = Just $ Unsafe.unsafeCoerce Refl
    | otherwise                             = Nothing
      -- as long as (type_ a) matches the value type of a—which the
      -- smart constructors for Attribute ensure—this is safe
      --
      -- there is presumably a way to do this without unsafeCoerce,
      -- but I do not know what it is

-- | When attributes have the same name, the /lower/ attribute
-- according to this ordering will take priority
--
-- In particular, this means that if the same name has a key with
-- @type_ = Nothing@ and a key with @type_ = Just ...@, the version
-- with @Nothing@ will take precedence.
--
-- When the same name has /multiple/ @type_ = Just typeRep@ keys the
-- ordering is unspecified—currently depends on the 'Ord' instance for
-- 'SomeTypeRep'.
instance GCompare (AttributeKey element namespace) where
  gcompare ka@(AttributeKey a) kb@(AttributeKey b)
    | Just Refl <- geq ka kb = GEQ
    | isLessThan             = GLT
    | otherwise              = GGT
    where isLessThan
            | name_a == name_b = Attribute.type_ a < Attribute.type_ b
            | otherwise        = name_a < name_b
          name_a = Attribute.name a
          name_b = Attribute.name b

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
-- class_ :: Attribute ["HTML", "SVG"] (Set ClassName)
-- href :: Attribute ["a", "area", "base", "link"] URL
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
data SetAttribute f element namespace where
  SetAttribute :: (KnownSymbols supports, Compatible element namespace supports)
               => Attribute supports a
               -> f a
               -> SetAttribute f element namespace

-- | Set an attribute to a static value that does /not/ change over
-- time.
--
-- __Example__
--
-- @
-- div [ class_ =: ["draggable", "card"] ]
-- @
(=:) :: ( Applicative f
        , KnownSymbols supports
        , Compatible element namespace supports
        )
     => Attribute supports a
     -- ^ attribute
     -> a
     -- ^ static value
     -> SetAttribute f element namespace
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
         , Compatible element namespace supports
         )
      => Attribute supports a
      -- ^ attribute
      -> f a
      -- ^ dynamic value
      -> SetAttribute f element namespace
(==:) = SetAttribute
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
--
-- Note: if there are multiple attributes with different types setting
-- the same name, only one value will be set:
--
--  * if one was set with 'override', it takes precdence over others
--
--  * if multiple attributes were set /without/ 'override', which one
--    takes precdence is unspecified
toDom :: forall f element namespace. Applicative f
      => AttributeSet f element namespace
      -> f (Map Text Text)
toDom (AttributeSet dmap) = DMap.foldlWithKey go (pure []) dmap
  where go :: forall v.
              f (Map Text Text)
           -> AttributeKey element namespace v
           -> f v
           -> f (Map Text Text)
        go existing (AttributeKey attribute) value =
          liftA2 (<>) converted existing
          where converted = Attribute.toAttributes attribute <$> value
