
-- Euler Project Problem 52 (Permuted multiples)

module Main where

import Data.List (sort)

main :: IO ()
main = print answer
  where
    answer = head $ concatMap test $ ([1..] :: [Int])

test :: Int -> [Integer]
test n = [ m | m <- [10^n .. (10^succ n `div` 6)]
             , let s = sort (show m)
             , s == sort (show (m * 2))
             , s == sort (show (m * 3))
             , s == sort (show (m * 4))
             , s == sort (show (m * 5))
             , s == sort (show (m * 6))
         ]
