
-- Euler Project Problem 3 (Largest prime factor)

module Main where

import Math.NumberTheory.Primes -- cabal install arithmoi

main :: IO ()
main = print answer
  where  answer = fst . last $ factorise' 600851475143
