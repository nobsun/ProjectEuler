
-- Euler Project Problem 35 (Circular primes)

module Main where

import Math.NumberTheory.Primes (primes, isPrime)  -- cabal install arithmoi

main :: IO ()
main = print answer
  where
    answer = 13 + length [ p | p <- base, circular p ]

base :: [Int]
base = map fromEnum $ takeWhile (1000000 >) $ dropWhile (100 >) primes

circular :: Int -> Bool
circular n = all isPrime 
           $ map (read . take len) 
           $ tail $ take len $ iterate tail $ cycle sn
  where
    sn = show n
    len = length sn
