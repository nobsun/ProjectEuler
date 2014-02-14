
-- Euler Project Problem 34 (Digit factorials)

module Main where

import Data.Char (digitToInt)

main :: IO ()
main = print answer
  where
    answer = sum [ n | n <- [11 .. fact 10]
                 , n == sum [ fact (digitToInt c) | c <- show n ] ]

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)
