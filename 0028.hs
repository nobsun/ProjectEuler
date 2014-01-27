
-- Euler Project Problem 28 (Number spiral diagonals)

module Main where

main :: IO ()
main = print answer
  where
    answer = succ $ sum $ map peri [1..500]

peri :: Int -> Int
peri n = (4*n^2 + n + 1) * 4
