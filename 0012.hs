
-- Euler Project Problem 12 (Highly divisible triangular number)

module Main where

import Math.NumberTheory.Primes (factorise') -- cabal install arithmoi

triangles :: [Integer]
triangles = scanl1 (+) [1..]
ndivisors :: Integer -> Int
ndivisors = product . map (succ . snd) . factorise'

main :: IO ()
main = print answer
  where
    answer = head $ snd $ break ((500<) . ndivisors) triangles

