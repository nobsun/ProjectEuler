
-- Euler Project Problem 33 (Digit canceling fractions)

module Main where

import Data.Ratio ((%),denominator)
import Data.List (intersect,(\\))

main :: IO ()
main = print answer
  where
    answer = denominator 
           $ product [ n % d | n <- [11..98]
                             , d <- [n+1 .. 99]
                             , n `mod` 10 /= 0 && d `mod` 10 /= 0
                             , let { sn = show n; sd = show d }
                             , not $ null $ intersect sn sd
                             , not $ null $ sn \\ sd
                             , n % d == read (sn \\ sd) % read (sd \\ sn)
                     ]        
