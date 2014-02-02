
-- Euler Project Problem 30 (Digit fifth powers)

module Main where

import Data.Char (digitToInt)

main :: IO ()
main = print answer
  where
    answer = sum [ x | x <- [2..1000000], x == sum (map ((^5) . digitToInt) (show x)) ]
