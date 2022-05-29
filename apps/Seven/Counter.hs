{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
module Seven.Counter where

import qualified Reflex
import qualified Reflex.Dom as Dom

import           UI.Element
import           UI.Widget

-- | The Counter has a frame with two components:
--
--  * a read-only label starting at 0
--  * a button that says "count"
--
-- Each time the button is pressed, the label goes up by 1.
widget :: Dom t m => m ()
widget = Dom.elClass "div" "counter" $ do
  rec output @Integer =<< Reflex.count clicks
      clicks <- Dom.button "count"
  pure ()
