{-# LANGUAGE OverloadedLists #-}
module Seven.CRUD where

import           Seven.Widget

import           Control.Monad   (void)

import qualified Data.ByteString as BS

import           Reflex.Dom

widget :: Dom t m => m ()
widget = void $ listbox (constDyn [(1::Int, "a"), (2, "b")])

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
