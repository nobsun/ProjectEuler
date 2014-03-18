
-- Euler Project Problem 70 (Totient permutation)

module Main where

import Data.List (sort)
import Math.NumberTheory.Primes (φ)
                                 
main :: IO ()
main = print answer

answer :: Integer
answer = snd $ minimum $ [ (toRational n / toRational phi, n) | n <- [2..9999999], let phi = φ n, sort (show n) == sort (show phi)]
