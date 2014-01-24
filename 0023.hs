
-- Euler Project Problem 23 (Non-abundant sums)

module Main where

import Data.List (sort, group, mapAccumL)
import Math.NumberTheory.Primes (divisorSum)

main :: IO ()
main = print answer
  where
    answer = sum $ snd $ mapAccumL check abundantSums [1..28123]
    check [] x                   = ([],x)
    check aas@(a:as) x | a >  x  = (aas,x)
                       | a == x  = (as, 0)
                       | a <  x  = check as x

abundantSums :: [Integer]
abundantSums = map head $ group $ sort 
             $ [ m + n | m <- abundants, n <- abundants, m <= n, m + n <= 28123]

abundants :: [Integer]
abundants = [ n | n <- [12 .. 28123], abundant n]

abundant :: Integer -> Bool
abundant n = 2 * n < divisorSum n
