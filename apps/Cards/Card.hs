-- | "Standard" or "French suited" 52-card deck playing cards.
--
-- The standard 52-card deck has four __suits__:
--
--  * spades (♠️)
--  * hearts (♥️)
--  * diamonds (♦️)
--  * clubs (♣️)
--
-- Each suit has 13 cards of different __ranks__:
--
--  * ace (A)
--  * numeric cards (2–10)
--  * jack (J)
--  * queen (Q)
--  * king (K)
--
module Cards.Card where

import qualified Data.Char              as Char
import           Data.Hashable          (Hashable)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Display      (Display (displayBuilder))
import qualified Data.Text.Lazy.Builder as Text
import           Data.Vector            ((!))
import qualified Data.Vector            as Vector

import           GHC.Generics           (Generic)

-- Doctest configuration (extensions/etc)
-- $
-- >>> :set -XOverloadedStrings

-- | The four suits that make up a standard 52-card deck:
--
--  * spades (♠️)
--  * hearts (♥️)
--  * diamonds (♦️)
--  * clubs (♣️)
--
-- Suits are ordered based on bridge conventions: ♣️ < ♦️ < ♥️ < ♠️, which
-- is the same as alphabetical order based on their English names.
--
-- >>> Spades > Clubs
-- True
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (Hashable)

-- | Colored symbols: ♠️, ♥️, ♦️, ♣️
instance Display Suit where
  displayBuilder = Text.fromText . suitShorthand

-- | All the suits in order.
--
-- >>> suits
-- [Clubs,Diamonds,Hearts,Spades]
suits :: [Suit]
suits = [minBound .. maxBound]

-- | The text name of each suit (in lowercase).
--
-- >>> suitName <$> suits
-- ["clubs","diamonds","hearts","spades"]
suitName :: Suit -> Text
suitName = Text.toLower . Text.pack . show

-- | The Unicode representation of each suit symbol /in color/.
--
--  * 'Spades': @♠️@
--  * 'Hearts': @♥️@
--  * 'Diamonds': @♦️@
--  * 'Clubs': @♣️@
--
-- Each symbol is a combination of two Unicode code points: the suit
-- character (@♠@, @♥@, @♢@ and @♣@) followed by @VARIATION
-- SELECTOR-16@ (#xfe0f) to make the symbol black or red as
-- appropriate.
--
-- >>> suitShorthand Hearts == "♥️"
-- True
suitShorthand :: Suit -> Text
suitShorthand Clubs    = "♣️"
suitShorthand Diamonds = "♦️"
suitShorthand Hearts   = "♥️"
suitShorthand Spades   = "♠️"

-- | Each suit has 13 cards with different __ranks__:
--
--  * ace (A)
--  * numeric cards (2–10)
--  * jack (J)
--  * queen (Q)
--  * king (K)
--
-- Ranks are ordered A < 2 < 3 < ... < 10 < J < Q < K
--
-- While the ordering for other cards is consistent in most conetexts,
-- some games have "aces high" (2 < ... < K < A) or let aces be /both/
-- high /and/ low (A < 2 ... < K < A). This makes the default ordering
-- choice for 'Rank' somewhat arbitrary, but having an 'Ord' instance
-- is useful enough to justify that.
--
-- >>> Ace < King
-- True
data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (Hashable)

-- | Conventional shorthand: A, 2–10, J, K, Q
instance Display Rank where
  displayBuilder = Text.fromText . rankShorthand

-- | All the ranks, ordered from 'Ace' to 'King'.
--
-- >>> ranks
-- [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]
ranks :: [Rank]
ranks = [minBound .. maxBound]

-- | The text name of each rank (in lowercase). Aces are @"ace"@ and
-- numbers are spelled out as words.
--
-- >>> rankName <$> ranks
-- ["ace","two","three","four","five","six","seven","eight","nine","ten","jack","queen","king"]
rankName :: Rank -> Text
rankName = Text.toLower . Text.pack . show

-- | The conventional shorthand for each rank:
--
--  * ace: @"A"@
--  * 2–10: @"2"@–@"10"@
--  * jack: @"J"@
--  * queen: @"Q"@
--  * king: @"K"@
rankShorthand :: Rank -> Text
rankShorthand Ace   = "A"
rankShorthand Two   = "2"
rankShorthand Three = "3"
rankShorthand Four  = "4"
rankShorthand Five  = "5"
rankShorthand Six   = "6"
rankShorthand Seven = "7"
rankShorthand Eight = "8"
rankShorthand Nine  = "9"
rankShorthand Ten   = "10"
rankShorthand Jack  = "J"
rankShorthand Queen = "Q"
rankShorthand King  = "K"

-- | One of the 52 cards in a standard deck, covering each combination
-- of rank and suit.
data Card = Card
  { rank :: !Rank
  , suit :: !Suit
  }
  deriving stock (Generic, Show, Eq, Ord, Bounded)
  deriving anyclass (Hashable)

-- | Standard shorthand, eg A♠️ for the ace of spades
instance Display Card where
  displayBuilder Card { rank, suit } =
    displayBuilder rank <> displayBuilder suit

-- | A list of all 52 playing cards in a standard deck.
--
-- Cards are ordered by suit first (♣️ < ♦️ < ♥️ < ♠️) then by rank (aces
-- low, kings high).
cards :: [Card]
cards = [Card rank suit | suit <- suits, rank <- ranks]

-- | The standard shorthand for a card: the rank followed by the suit
-- (eg A♠️ for the ace of spades).
--
-- See also: 'rankShorthand' and 'suitShorthand'
--
-- >>> cardShorthand (Card Ace Spades) == "A♠️"
-- True
--
-- >>> cardShorthand (Card Ten Clubs) == "10♣️"
-- True
--
-- >>> cardShorthand (Card King Hearts) == "K♥️"
-- True
cardShorthand :: Card -> Text
cardShorthand Card { rank, suit } = rankShorthand rank <> suitShorthand suit

-- | The Unicode 7.0 character for each card (🂡, 🃊, 🃞... etc)
--
-- Note that these characters do not render well at smaller sizes in
-- most fonts.
--
-- >>> cardCharacter (Card Ace Spades) == '🂡'
-- True
--
-- >>> cardCharacter (Card Jack Diamonds) == '🃋'
-- True
--
-- >>> cardCharacter (Card King Hearts) == '🂾'
-- True
cardCharacter :: Card -> Char
cardCharacter Card { rank, suit } = case suit of
  Clubs    -> clubs    ! fromEnum rank
  Diamonds -> diamonds ! fromEnum rank
  Hearts   -> hearts   ! fromEnum rank
  Spades   -> spades   ! fromEnum rank
  where clubs    = chars 0x1F0D1
        diamonds = chars 0x1F0C1
        hearts   = chars 0x1F0B1
        spades   = chars 0x1F0A1

        -- starting from code point for the ace of each suit
        chars ace = Vector.fromList
          [ Char.chr x
          | x <- [ace .. ace + 13]
          , x `mod` 16 /= 0xC -- skip "knight" card
          ]
