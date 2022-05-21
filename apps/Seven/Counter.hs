{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
module Seven.Counter where

import           Seven.Element
import           Seven.Widget

import           Control.Monad.Fix (MonadFix)

import qualified Data.Text         as Text

import           Reflex
import           Reflex.Dom

-- | The Counter has a frame with two components:
--
--  * a read-only label starting at 0
--  * a button that says "count"
--
-- Each time the button is pressed, the label goes up by 1.
widget :: Dom t m => m ()
widget = elClass "div" "counter" $ do
  rec output @Integer =<< count clicks
      clicks <- button "count"
  pure ()
