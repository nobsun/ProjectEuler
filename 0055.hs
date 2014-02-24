
-- Euler Project Problem 55 (Lychrel numbers)

module Main where

import Control.Applicative ((<*>))
import Data.List (find)
import Data.Maybe (mapMaybe)

main :: IO ()
main = print answer
  where
    answer = (9999 -) $ length $ mapMaybe proc [1..9999]

isPalindrome :: Integer -> Bool
isPalindrome = ((==) <*> reverse) . show

revIntSum :: Integer -> Integer
revIntSum = (+) <*> (read . reverse . show)

proc :: Integer -> Maybe Integer
proc = find isPalindrome . take 50 . iterate revIntSum . revIntSum
