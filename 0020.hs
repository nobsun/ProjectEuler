
-- Euler Project Problem 20 (Factorial digit sum)

module Main where

import Data.Char (digitToInt)

main :: IO ()
main = print answer
  where
    answer = sum $ map digitToInt $ show $ product [1..100]
