module Main where

import App
import System.Environment

main :: IO ()
main = do
  port <- getArgs
  run "sqlite.db" (head port)
