
-- Euler Project Problem 25 (1000-digit Fibonacci number)

module Main where

main :: IO ()
main = print answer
  where
    answer = length $ takeWhile ((1000 >) . length . show) fibs

fibs :: [Integer]
fibs@(_:fibs') = 0:1:zipWith (+) fibs fibs'

