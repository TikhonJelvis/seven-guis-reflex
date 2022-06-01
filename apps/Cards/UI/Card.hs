{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cards.UI.Card where

import           Cards.Card         (Card (..), Rank (..), Suit (..), rankName,
                                     suitName)

import           Control.Monad      (void)

import           Data.Default.Class (def)
import           Data.Map           (Map)
import           Data.Maybe         (fromMaybe, isJust)
import           Data.Text          (Text)

import           Reflex             (Dynamic)
import qualified Reflex.Dom         as Dom

import qualified Text.URI           as URI

import           UI.Attributes      (addClass, removeClass)
import qualified UI.Drag            as Drag
import           UI.Drag            (Drags (..))
import           UI.Element         (Dom, Element, elClass', elDynAttr')
import           UI.Main            (Runnable (..), withCss)
import           UI.Style           (Angle (..), rotate, setProperty, translate)
import           UI.Widget          (image, label)

demo :: forall m t. Dom t m => m ()
demo = void $ elClass' "div" "card-demo" do
  label "Cards draggable across body"
  draggable $ Card Ace Spades
  draggable $ Card King Hearts
  pure ()
  where draggable c = do
          rec element <- card c attributes
              let attributes = do
                    move <- translate <$> total
                    class' <- whenDragged . isJust <$> current
                    pure $ class' $ move [("class", "draggable")]
              Drags { total, current } <- Drag.drags def element
          pure ()

        whenDragged True  =
          setProperty "z-index" "100" . rotate (Deg 5) . addClass "dragging"
        whenDragged False = removeClass "dragging"

-- | Creates an element representing a specific card.
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
card :: forall m t. Dom t m
     => Card
     -> Dynamic t (Map Text Text)
     -- ^ Additional attributes for the element.
     --
     -- Note: the @card@, rank and suit classes will be added in
     -- addition to any classes specified by these attributes.
     -> m (Element t)
card card'@Card { rank, suit } attributes = do
  (element, _) <- elDynAttr' "div" (withClasses <$> attributes) do
    -- top, with rank + suit in lefthand corner
    -- Dom.elClass "div" "top" summary

    -- core part of card with pips or picture
    Dom.elClass "div" "center" $ cardCenter card'

    -- bottom, with rank + suit in righthand corner
    -- Dom.elClass "div" "bottom" summary
  pure element
  where withClasses =
          addClass "card" .
          addClass (suitName suit) .
          addClass (rankName rank)

        -- summary = do
        --   Dom.elClass "div" "rank-summary" $ Dom.text $ rankShorthand rank
        --   Dom.elClass "div" "suit-summary" $ Dom.text $ suitShorthand suit

-- | Render the center part of a card which contains either pips or a
-- picture as appropriate.
cardCenter :: forall m t. Dom t m => Card -> m ()
cardCenter Card { rank, suit } =
  void $ image $ fromMaybe (error "Invalid URI!") $ URI.mkURI cardURI
  where cardURI = "img/" <> rankName rank <> "-" <> suitName suit <> ".svg"

main :: IO ()
main = withCss "css/card-demo.css" (Runnable demo)
