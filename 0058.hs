
-- Euler Project Problem 58 (Spiral primes)

module Main where

import Control.Arrow ((&&&))
import Data.List (genericLength)
import Math.NumberTheory.Primes (isPrime)

main :: IO ()
main = print answer
  where
    answer = info $ dropWhile check $ uncurry zip $ (tail . scanl (aggr diagPrimes) 0 &&& map (succ . (4*))) [1..]
    aggr f s n = s + f n

diagPrimes :: Integer -> Integer
diagPrimes n = genericLength $ filter isPrime $ [sq - e*3, sq - e*2, sq - e]
  where
    sq = (e+1)^2
    e  = 2*n

check :: (Integer, Integer) -> Bool
check (n,d) = 10*n >= d

info :: [(Integer,Integer)] -> Integer
info ((_,n):_) = (n - 1) `div` 2 + 1