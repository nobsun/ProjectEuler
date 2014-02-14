
-- Euler Project Problem 42 (Coded triangle numbers)

module Main where

import Data.Char (ord)
import Data.List (foldl')

main :: IO ()
main = print . answer =<< readFile "words.txt"
  where  answer = length . filter (isTriangle) . map (wordToNum . rmQuote) . words . map comma2space

triangles :: [Int]
triangles = scanl1 (+) [1..]

isTriangle :: Int -> Bool
isTriangle = member triangles
  where
    member (x:xs) n
     | x <  n    =  member xs n
     | x >  n    =  False
     | otherwise =  True

wordToNum :: String -> Int
wordToNum = foldl' (\ s c -> s + charToNum c) 0 

charToNum    ::  Char -> Int
charToNum c  =   ord c - ord 'A' + 1

rmQuote :: String -> String
rmQuote = filter ('"'/=)

comma2space :: Char -> Char
comma2space ',' = ' '
comma2space c   = c
