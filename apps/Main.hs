module Main where

import qualified Counter
import           Reflex.Dom (el, elAttr, mainWidget, text, (=:))

main :: IO ()
main = mainWidget Counter.widget
