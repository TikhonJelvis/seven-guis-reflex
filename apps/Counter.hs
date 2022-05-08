{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
module Counter where

import           Control.Monad.Fix (MonadFix)

import qualified Data.Text         as Text

import           Reflex
import           Reflex.Dom

import           Widget

-- | The Counter has a frame with two components:
--
--  * a read-only label starting at 0
--  * a button that says "count"
--
-- Each time the button is pressed, the label goes up by 1.
widget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m) => m ()
widget = elClass "div" "counter" $ do
  rec output @Integer =<< count clicks
      clicks <- button "count"
  pure ()
