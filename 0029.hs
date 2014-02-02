
-- Euler Project Problem 29 (Distinct powers)

module Main where

import Data.List (group, sort)

main :: IO ()
main = print answer
  where
    answer = length $ group $ sort $ [a^b | let cs = [2..100], a <- cs, b <- cs]
