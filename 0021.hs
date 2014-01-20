
-- Euler Project Problem 21 (Amicable numbers)

module Main where

import Math.NumberTheory.Primes

main :: IO ()
main = print answer
  where
    answer = sum [ n | n <- [2..10000], amicable n ]

amicable :: Integer -> Bool
amicable m = m /= n && divisorSum n - n == m
  where
    n = divisorSum m - m
