module Main where

import qualified Cards.UI.Card as Card

import           UI.Main       (Runnable (Runnable), withCss)


main :: IO ()
main = withCss "css/card-demo.css" (Runnable Card.demo)
