module UI.PushMap where

import           Data.Bifunctor (second)
import           Data.Hashable  (Hashable)
import           Data.Map       (Map)
import qualified Data.Map       as Map

import           GHC.Exts       (IsList (..))
import           GHC.Generics   (Generic)

import           Prelude        hiding (lookup)

import           Witherable     (Filterable)

-- | A container that assigns increasing 'Int' indices when a value is
-- pushed to it.
--
-- The first element pushed to the container gets the index
-- @0@. Subsequent elements will always get an index larger than any
-- index currently assigned, but with no guarantees about the exact
-- progression.
newtype PushMap a = PushMap { toMap :: Map Int a }
  deriving stock (Show, Eq, Foldable, Functor, Generic)
  deriving newtype (Semigroup, Monoid, Filterable)
  deriving anyclass (Hashable)

instance IsList (PushMap a) where
  type Item (PushMap a) = (Int, a)

  fromList xs = PushMap $ Map.fromList xs
  toList (PushMap m) = Map.toList m

-- | Push a new value onto the map, giving it a larger index than any
-- of the existing elements.
--
-- >>> push "foo" []
-- PushMap {toMap = fromList [(0,"foo")]}
--
-- >>> push "bar" [(5, "foo")]
-- PushMap {toMap = fromList [(5,"foo"),(6,"bar")]}
push :: a -> PushMap a -> PushMap a
push a (PushMap m)
  | Map.null m = PushMap [(0, a)]
  | otherwise  = PushMap $ Map.insert (fst (Map.findMax m) + 1) a m

-- | Delete and return the value at the largest index in the map.
--
-- Returns 'Nothing' if the map is empty.
--
-- >>> pop [(0, "foo"), (1, "bar")]
-- Just ("bar",PushMap {toMap = fromList [(0,"foo")]})
--
-- >>> pop []
-- Nothing
pop :: PushMap a -> Maybe (a, PushMap a)
pop (PushMap m) = second PushMap <$> Map.maxView m

-- | Set the value associated with the given key.
--
-- >>> insert 4 "foo" []
-- PushMap {toMap = fromList [(4,"foo")]}
--
-- >>> insert 4 "bar" [(4, "foo")]
-- PushMap {toMap = fromList [(4,"bar")]}
insert :: Int -> a -> PushMap a -> PushMap a
insert k a (PushMap m) = PushMap $ Map.insert k a m

-- | Returns the largest 'Int' key assigned to a value in the given
-- 'PushMap'.
--
-- >>> maxKey []
-- Nothing
--
-- >>> maxKey [(0, "foo"), (100, "bar")]
-- Just 100
maxKey :: PushMap a -> Maybe Int
maxKey (PushMap m)
  | Map.null m = Nothing
  | otherwise  = Just $ fst $ Map.findMax m

-- | Look up the value assigned to the given 'Int' key, if any.
--
-- >>> lookup 1 []
-- Nothing
--
-- >>> lookup 1 [(0, "foo"), (1, "bar")]
-- Just "bar"
lookup :: Int -> PushMap a -> Maybe a
lookup i (PushMap m) = Map.lookup i m

-- | Remove the item at the given index from the map.
--
-- Unless the removed index was the largest index in the map, this
-- does not change what index the next item inserted into the map will
-- receive.
--
-- >>> delete 1 [(0, "foo"), (1, "bar")]
-- PushMap {toMap = fromList [(0,"foo")]}
--
-- >>> delete 1 [(0, "foo")]
-- PushMap {toMap = fromList [(0,"foo")]}
delete :: Int -> PushMap a -> PushMap a
delete i (PushMap m) = PushMap $ Map.delete i m
