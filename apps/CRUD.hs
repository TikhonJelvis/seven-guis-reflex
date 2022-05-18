{-# LANGUAGE OverloadedLists #-}
module CRUD where

import           Control.Monad   (void)

import qualified Data.ByteString as BS

import           Reflex.Dom

import           Widget

widget :: Dom t m => m ()
widget = void $ listbox (constDyn [(1::Int, "a"), (2, "b")])

main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css widget
