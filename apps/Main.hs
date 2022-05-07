{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.FileEmbed (embedFile)
import qualified Data.Text      as Text

import           Text.Printf    (printf)

import           Reflex.Dom     (mainWidgetWithCss)

import qualified Counter

main :: IO ()
main = mainWidgetWithCss css Counter.widget
  where css = $(embedFile "css/tasks.css")
