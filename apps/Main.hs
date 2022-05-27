module Main where

import qualified Seven.CRUD      as CRUD

import qualified Data.ByteString as BS

import           Reflex.Dom      (mainWidgetWithCss)


main :: IO ()
main = do
  css <- BS.readFile "css/tasks.css"
  mainWidgetWithCss css CRUD.widget
