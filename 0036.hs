
-- Euler Project Problem 36 (Double-base palindromes)

module Main where

import Control.Applicative ((<*>))
import Data.List (unfoldr)

main :: IO ()
main = print answer
  where
    answer = sum [ x | x <- [1,3 .. 999999], pal10 x && pal2 x ]

palindrome :: Eq a => [a] -> Bool
palindrome = (==) <*> reverse

pal10 :: Int -> Bool
pal10 = palindrome . show

pal2 :: Int -> Bool
pal2 = palindrome . toBits

toBits :: Int -> [Int]
toBits = unfoldr phi
  where
    phi 0 = Nothing
    phi n = Just $ swap $ divMod n 2

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
