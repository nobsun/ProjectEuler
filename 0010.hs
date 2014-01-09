
-- Euler Project Problem 10 (Summation of primes)

module Main where

import Math.NumberTheory.Primes.Sieve (primes) -- cabal install arithmoi

main :: IO ()
main = print answer
  where
    answer = sum $ takeWhile (2000000>) primes
