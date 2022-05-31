module Main where

import qualified Seven.CRUD as CRUD

import           UI.Main    (Runnable (Runnable), withCss)


main :: IO ()
main = withCss "css/tasks.css" (Runnable CRUD.widget)
