
-- Euler Project Problem 27 (Quadratic primes)

module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Math.NumberTheory.Primes (primes,isPrime)

main :: IO ()
main = print answer
  where
    answer = uncurry (*) $ maximumBy (comparing count) [(a,b) | a <- cs, b <- ps, isPrime (1+a+b)]
    cs = [-999 .. 999]
    ps = takeWhile (1000 >) primes

count :: (Integer,Integer) -> Integer
count (a,b) = iter 1
  where
    iter n | isPrime (n^2 + a*n + b) = iter (succ n)
           | otherwise               = n
