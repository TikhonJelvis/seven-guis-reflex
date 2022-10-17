{-# LANGUAGE UndecidableInstances #-}
module UI.Css.Rules where

import           Data.Hashable           (Hashable)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Display       (Display)
import           Data.Typeable           (Typeable)

import           GHC.Exts                (IsList)
import           GHC.Generics            (Generic)

import qualified UI.Attributes           as Attributes
import           UI.Attributes           (Attribute)
import           UI.Attributes.Attribute (AsAttributeValue (..),
                                          CombineAttributeValue (..))

-- * Properties

-- | A CSS property name.
newtype Property = Property Text
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Display, IsString, Hashable)

-- * Rules

-- TODO: structured representation of CSS rules?
-- | A set of CSS rules for a single element.
--
-- When the attribute is set multiple times, the map of rules is
-- combined. If the same /rule/ ends up being set multiple times, the
-- newer definition takes precendence.
newtype CssRules = CssRules (Map Property Text)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)
  deriving newtype (IsList, Semigroup, Monoid)

instance CombineAttributeValue CssRules where
   -- we want *newer* values (second argument) to take priority
  combineAttributeValues = flip (<>)

instance AsAttributeValue CssRules where
  toAttributeValue (CssRules (Map.toList -> rules)) =
    Text.intercalate ";" [ property <> " : " <> value
                         | (Property property, value) <- rules ]

    -- TODO: proper CSS parsing?
  fromAttributeValue text = Just $ CssRules $ Map.fromList
    [ toKV declaration
    | declaration <- Text.strip <$> Text.split (== ';') text
    , declaration /= ""
    ]
    where toKV declaration =
            let (a, b) = Text.break (== ':') declaration
            in (Property $ Text.strip a, Text.strip $ Text.drop 1 b)

-- * Manipulating CSS Rules

-- | Apply an operation over a set of CSS rules encoded in text.
withRules :: (Maybe CssRules -> CssRules) -> Text -> Text
withRules f rules = toAttributeValue $ f (fromAttributeValue rules)

    -- TODO: types CSS rules based on Attribute + AttributeSet
-- | Set a property to a given value.
--
-- This will overwrite the previous value /of that exact property/,
-- but will not touch related properties. Setting @border: 1px@ will
-- override the previous @border@ value but will not affect properties
-- like @border-width@ or @border-right@.
--
-- >>> setProperty "color" "blue" [("pointer", "auto")]
-- CssRules (fromList [("color","blue"),("pointer","auto")])
--
-- >>> setProperty "color" "blue" [("pointer", "auto"), ("color", "green"), ("background-color", "yellow")]
-- CssRules (fromList [("background-color","yellow"),("color","blue"),("pointer","auto")])
--
-- >>> setProperty "color" "blue" []
-- CssRules (fromList [("color","blue")])
setProperty :: forall a. AsAttributeValue a
            => Property
            -- ^ Property name
            -> a
            -- ^ Property value
            -> CssRules
            -> CssRules
setProperty property value (CssRules rules) =
  CssRules $ Map.insert property (toAttributeValue value) rules

-- | Use the given function to combine the old value of a property
-- with the given new value.
--
-- If the property is not set, this will set it to the given value.
--
-- This is handy for properties that can take a sequence of values
-- like @transform@:
--
-- >>> let app b a = a <> " " <> b
-- >>> let translate = "translate(10px, 10px)"
--
-- >>> updateProperty app "transform" translate []
-- CssRules (fromList [("transform","translate(10px, 10px)")])
--
-- >>> updateProperty app "transform" translate [("style", "transform: rotate(10deg)")]
-- CssRules (fromList [("style","transform: rotate(10deg)"),("transform","translate(10px, 10px)")])
updateProperty :: (Text -> Text -> Text)
               -- ^ Function to combine values: @f new old@
               -> Property
               -- ^ Property name
               -> Text
               -- ^ New property value
               -> CssRules
               -> CssRules
updateProperty f property value (CssRules rules) =
  CssRules $ Map.insertWith f property value rules

-- | Remove the given property from the set of rules, if present.
deleteProperty :: Property -> CssRules -> CssRules
deleteProperty property (CssRules rules) = CssRules $ Map.delete property rules

-- | Set the @user-select@ and @-webkit-user-select@ properties.
--
-- While WebKit /should/ support @user-select@ with no prefix, only
-- the prefixed version worked for me in WebKitGTK.
setUserSelect :: Text -> CssRules -> CssRules
setUserSelect value =
  setProperty "user-select" value . setProperty "-webkit-user-select" value

-- * Attributes


-- ** Attributes for Properties

-- $ CSS properties on elements can be set in one of two ways:
--
--  1. Through explicit attributes like 'zIndex'
--  2. Through the 'style' attribute
--
-- If the same property is set through both an explicit attribute
-- /and/ 'style', the value from the explicit attribute will take
-- priority. It is good practice to use explicit attributes whenever
-- they are defined and only rely on 'style' as a "backdoor" for
-- properties that are either not supported in the library or
-- browser/domain-specific.

-- | Set the CSS styles for an element.
style :: Attribute CssRules
style = Attributes.logical "style" \ existing new ->
  let go _ = withRules \case
        Just old -> old <> new -- old takes precedence
        Nothing  -> new
  in Map.insertWith go "style" (toAttributeValue new) existing

-- | Create a new top-level attribute for a CSS property.
--
-- The way that values get mapped to/from DOM strings and how multiple
-- instances get combined is determined by the type's
-- 'AsAttributeValue' instance.
--
-- If the value would map to an empty string (@toAttributeValue x ==
-- ""@), the property is removed from the @style@ attribute.
--
-- __Example__
--
-- @
-- zIndex :: Attribute Int
-- zIndex = css "z-index"
-- @
css :: forall a. (AsAttributeValue a, Typeable a) => Property -> Attribute a
css property@(Property name) = Attributes.logical name \ existing new ->
  case toAttributeValue new of
    "" -> Map.update clearProperty "style" existing
    newValue ->
      let newRules = [(property, newValue)]
      in Map.insertWith (go newRules) "style" (toAttributeValue newRules) existing
  where go new _ = withRules \case
          Just old -> new <> old
          Nothing  -> new

        clearProperty oldStyle =
          toAttributeValue . deleteProperty property <$> fromAttributeValue oldStyle
