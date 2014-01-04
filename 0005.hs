
-- Euler Project Problem 5 (Smallest multiple)

module Main where

import Data.List (foldl1')

main :: IO ()
main = print answer
  where
    answer = foldl1' lcm [1..20]

