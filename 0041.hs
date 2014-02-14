
-- Euler Project Problem 41 (Pandigital prime)

module Main where

import Data.List (permutations)
import Math.NumberTheory.Primes -- cabal install arithmoi

main :: IO ()
main = print answer
  where  answer = maximum [ x | x <- map read (permutations "1234567") , isPrime x ]

