{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Cards.UI.Card where

import           Cards.Card         (Card (..), Rank (..), Suit (..), rankName,
                                     suitName)

import           Control.Monad      (void)

import           Data.Default.Class (Default, def)
import           Data.Map           (Map)
import           Data.Maybe         (fromMaybe, isJust)
import           Data.Text          (Text)

import           GHC.Generics       (Generic)

import           Reflex             (Dynamic)
import qualified Reflex.Dom         as Dom

import qualified Text.URI           as URI

import           UI.Attributes      (addClass, removeClass, setClass,
                                     setProperty)
import qualified UI.Drag            as Drag
import           UI.Drag            (DragConfig (..), Drags (..))
import           UI.Element         (Dom, Html, elClass', elDynAttr')
import           UI.Main            (Runnable (..), withCss)
import           UI.Style           (Angle (..), rotate, translate)
import           UI.Widget          (Enabled (..), image, label)

demo :: forall m t. Dom t m => m ()
demo = void $ elClass' "div" "card-demo" do
  label "Cards draggable across body"
  rec CardElement { drags } <-
        draggable (Card Ace Spades) def { attributes = attributes' drags }
  rec CardElement { drags } <-
        draggable (Card King Hearts) def { attributes = attributes' drags }
  pure ()
  where attributes' Drags { current } =
          Just $ whenDragged . isJust <$> current <*> pure []
        whenDragged True  =
          setProperty "z-index" "100" . rotate (Deg 5) . addClass "dragging"
        whenDragged False = removeClass "dragging"

-- | Various parameters that can be set up for a card element.
data CardConfig t = CardConfig
  { container  :: Maybe (Html t)
    -- ^ The container within which the card is draggable.
    --
    -- Drag events outside the container will not register.

  , dragging   :: Maybe (Dynamic t Enabled)
    -- ^ Is dragging enabled for the card?

  , attributes :: Maybe (Dynamic t (Map Text Text))
    -- ^ Any additional dynamic attributes.
    --
    -- Note that various card-specific properties will add to or
    -- override properties set here.
  }
  deriving stock (Generic)

-- | All values default to 'Nothing'
instance Default (CardConfig t) where

-- | Information about a card element.
data CardElement t = CardElement
  { drags :: Drags t
  , card  :: Cards.Card.Card
  }
  deriving stock (Generic)

-- | Creates a draggable element representing a specific card.
--
-- The element will have the CSS class @card@ as well as classes for
-- its rank (@ace@, @two@... @ten@, @jack@, @queen@, @king@) and suit
-- (@clubs@, @diamonds@, @hearts@, @spades@).
--
-- __Examples__
--
-- An ace of spades that can be dragged around the body of the
-- document:
--
-- @
-- ace :: m (Element t)
-- ace = do
--   rec element <- card (Card Ace Spades) attributes
--       let attributes = translate <$> total <*> pure []
--       Drags { total } <- Drag.drags def element
--   pure element
-- @
draggable :: forall m t. Dom t m
          => Card
          -> CardConfig t
          -> m (CardElement t)
draggable card@Cards.Card.Card { rank, suit } CardConfig { container, dragging, attributes } = mdo
  (element, _) <- elDynAttr' "div" attributes' do
    -- core part of card with pips or picture
    Dom.elClass "div" "center" $ cardCenter card

  drags@Drags { current, total } <-
    Drag.drags def { container, enabled = dragging } element
  let attributes' = do
        move <- translate <$> total
        draggingClass <- setClass "dragging" . isJust <$> current
        move . draggingClass . classes <$> fromMaybe (pure []) attributes

  pure CardElement { card, drags }
  where classes =
          addClass "draggable" .
          addClass "card" .
          addClass (Cards.Card.suitName suit) .
          addClass (Cards.Card.rankName rank)

        -- summary = do
        --   Dom.elClass "div" "rank-summary" $ Dom.text $ rankShorthand rank
        --   Dom.elClass "div" "suit-summary" $ Dom.text $ suitShorthand suit

-- | Render the center part of a card which contains either pips or a
-- picture as appropriate.
cardCenter :: forall m t. Dom t m => Cards.Card.Card -> m ()
cardCenter Cards.Card.Card { rank, suit } =
  void $ image $ fromMaybe (error "Invalid URI!") $ URI.mkURI cardURI
  where cardURI = "img/" <> Cards.Card.rankName rank <> "-" <> Cards.Card.suitName suit <> ".svg"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
