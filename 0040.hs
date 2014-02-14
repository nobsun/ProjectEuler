
-- Euler Project Problem 40 (Champernowne's constant)

module Main where

import Data.Char (digitToInt)
import Data.List (mapAccumL)

main :: IO ()
main = print answer
  where
    answer = product $ map digitToInt $ snd 
           $ mapAccumL f (concat $ map show [0..]) [1,9,90,900,9000,90000,900000]
    f a x = (drop x a, a !! x)
