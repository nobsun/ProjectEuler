
-- Euler Project Problem 56 (Powerful digit sum)

module Main where

import Data.Char (digitToInt)

main :: IO ()
main = print answer
  where
    answer = maximum [ sum $ map digitToInt $ show (a^b) | a <- [1..99], b <- [1..99]]
