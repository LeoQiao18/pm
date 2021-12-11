module Main where

import           PM

-- cli options:
-- 1. depth of search
-- 2. interactive / play one move using board from file
main :: IO ()
main = putStrLn $ "Player: " ++ show Black

-- get board
-- get next move for board
