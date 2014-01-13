
-- Euler Project Problem 14 (Longest Collatz sequence)

module Main where

import Data.List (maximumBy)
import Data.Ord

collatzCount :: Integer -> Int
collatzCount = iter 1
  where
    iter c 1 = c
    iter c n | even n     = iter (succ c) (n `div` 2)
             | otherwise  = iter (succ c) (3 * n + 1)

main :: IO ()
main = print answer
  where
    answer = maximumBy (comparing collatzCount) [1..999999]
