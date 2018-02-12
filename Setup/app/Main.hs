module Main where

import Lib

main :: IO ()
main = do
  tellBecomeGroup "localhost" 4000
--  tellBecomeGroup "localhost" 4001
  tellJoinGroup "localhost" 5000
--  tellJoinGroup "localhost" 5001
--  tellJoinGroup "localhost" 5002
