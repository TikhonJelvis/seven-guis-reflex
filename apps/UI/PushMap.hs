{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeFamilies               #-}
module UI.PushMap where

import           Control.Lens   ((<&>))

import           Data.Bifunctor (second)
import           Data.Map       (Map)
import qualified Data.Map       as Map

import           GHC.Exts       (IsList (..))

import           Prelude        hiding (lookup)

import           Witherable     (Filterable, Witherable)

-- | A container that assigns increasing 'Int' indices when a value is
-- pushed to it.
--
-- The first element pushed to the container gets the index
-- @0@. Subsequent elements will always get an index larger than any
-- index currently assigned, but with no guarantees about the exact
-- progression.
newtype PushMap a = PushMap { toMap :: Map Int a }
  deriving stock (Show, Eq, Foldable, Functor)
  deriving newtype (Semigroup, Monoid, Filterable)

instance IsList (PushMap a) where
  type Item (PushMap a) = (Int, a)

  fromList xs = PushMap $ Map.fromList xs
  toList (PushMap map) = Map.toList map

-- | Push a new value onto the map, giving it a larger index than any
-- of the existing elements.
--
-- >>> push "foo" []
-- PushMap {toMap = fromList [(0,"foo")]}
--
-- >>> push "bar" [(5, "foo")]
-- PushMap {toMap = fromList [(5,"foo"),(6,"bar")]}
push :: a -> PushMap a -> PushMap a
push a (PushMap map)
  | Map.null map = PushMap [(0, a)]
  | otherwise    = PushMap $ Map.insert (fst (Map.findMax map) + 1) a map

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
pop (PushMap map) = second PushMap <$> Map.maxView map

-- | Set the value associated with the given key.
--
-- >>> insert 4 "foo" []
-- PushMap {toMap = fromList [(4,"foo")]}
--
-- >>> insert 4 "bar" [(4, "foo")]
-- PushMap {toMap = fromList [(4,"bar")]}
insert :: Int -> a -> PushMap a -> PushMap a
insert k a (PushMap map) = PushMap $ Map.insert k a map

-- | Returns the largest 'Int' key assigned to a value in the given
-- 'PushMap'.
--
-- >>> maxKey []
-- Nothing
--
-- >>> maxKey [(0, "foo"), (100, "bar")]
-- Just 100
maxKey :: PushMap a -> Maybe Int
maxKey (PushMap map)
  | Map.null map = Nothing
  | otherwise    = Just $ fst $ Map.findMax map

-- | Look up the value assigned to the given 'Int' key, if any.
--
-- >>> lookup 1 []
-- Nothing
--
-- >>> lookup 1 [(0, "foo"), (1, "bar")]
-- Just "bar"
lookup :: Int -> PushMap a -> Maybe a
lookup i (PushMap map) = Map.lookup i map

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
delete i (PushMap map) = PushMap $ Map.delete i map
