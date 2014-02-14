
-- Euler Project Problem 37 (Truncatable primes)

module Main where

import Data.List (tails,inits)
import Math.NumberTheory.Primes (primes, isPrime) -- cabal install arithmoi

main :: IO ()
main = print answer
  where
    answer = sum $ take 11 $ [ p | p <- drop 4 primes, trancr p && trancl p ]

trancr :: Integer -> Bool
trancr = all isPrime . map read . init . tails . show

trancl :: Integer -> Bool
trancl = all isPrime . map read . tail . inits . show
