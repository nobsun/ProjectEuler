
-- Euler Project Problem 73 (Counting fractions in a range)

module Main where

import Data.Ratio ((%))

main :: IO ()
main = print answer

answer :: Int
answer = length [ n % d | n <- [2..6000], d <- [n*2+1 .. min 12000 (n*3-1)], gcd n d == 1 ]
