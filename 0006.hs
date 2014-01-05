
-- Euler Project Problem 6 (Sum square difference)

module Main where

sumOfSquares :: Int -> Int
sumOfSquares n = n * (n+1) * (n+n+1) `div` 6

squareOfSum :: Int -> Int
squareOfSum n = s * s
  where  s = n * (n+1) `div` 2

main :: IO ()
main = print answer
  where
    answer = squareOfSum 100 - sumOfSquares 100

