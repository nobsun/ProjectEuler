
-- Euler Project Problem 7 (10001st prime)

module Main where

import Math.NumberTheory.Primes.Counting (nthPrime) -- cabal install arithmoi

main :: IO ()
main = print answer
  where
    answer = nthPrime 10001
