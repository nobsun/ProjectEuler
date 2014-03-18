
-- Euler Project Problem 72 (Counting fractions)

module Main where

import Math.NumberTheory.Primes (φ)

main :: IO ()
main = print (answer 1000000)

answer :: Integer -> Integer
answer n = pred (sum (map φ [1..n]))
