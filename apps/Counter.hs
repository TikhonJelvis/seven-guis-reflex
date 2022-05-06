{-# LANGUAGE OverloadedStrings #-}
module Counter where

import           Reflex.Dom (DomBuilder, blank, divClass, dtdd, el, elAttr,
                             elClass, text, (=:))

widget :: DomBuilder t m => m ()
widget = do
  el "div" $
    elAttr "span" ("style" =: "color:blue") $ text "Hello, World!"

  -- Use CSS style center-align and red-text
  -- using these specialised APIs
  divClass "center-align" $
    elClass "span" "red-text" $
      text "Div with class center-align and red text"

  el "dl" $ do
    dtdd "dt dd tags" $
      text "Here goes the description"

    dtdd "Reflex" $ do
      text "Haskell + awesome FRP!"
      el "br" blank -- Add line break, blank == return ()
      -- A simple URL link
      elAttr "a" ("href" =: "http://reflexfrp.org") (text "Reflex-FRP")
