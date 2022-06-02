{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Many games involve placing cards face-up or face-down on a
-- playing area, or putting cards in a pile.
module Cards.Place where

import           Data.Hashable         (Hashable)
import           Data.Vector           (Vector)
import           Data.Vector.Instances ()

import           GHC.Exts              (IsList (..))
import           GHC.Generics          (Generic)

-- | Whether a card is placed face-up or face-down.
data Face = FaceUp
          -- ^ A card placed face-up means that its rank and suit are
          -- known to everybody.
          | FaceDown
          -- ^ A card placed face-down means that its rank and suit is
          -- /not/ automatically known to everybody.
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

-- | A card placed on the playing area, either face-up or face-down.
data Place a = Place
  { status :: Face
  -- ^ Is the card placed face-up or face-down?
  , card   :: a
  -- ^ The card itself.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A pile of cards placed on on top of the other.
newtype Pile a = Pile (Vector a)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance IsList (Pile a) where
  type Item (Pile a) = a

  fromList = Pile . fromList
  toList (Pile cards) = toList cards

