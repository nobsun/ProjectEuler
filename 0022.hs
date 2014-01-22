
-- Euler Project Problem 22 (Names scores)

module Main where

import Data.Char (ord)
import Data.List (sort)

main :: IO ()
main = print . answer =<< readFile "names.txt"
  where
    answer = sum . zipWith score [1..] . sort . map rmQuote . words . map comma2space

rmQuote :: String -> String
rmQuote = filter ('"'/=)

comma2space :: Char -> Char
comma2space ',' = ' '
comma2space c   = c

score :: Int -> String -> Int
score i s = i * sum [ ord c - ord 'A' + 1 | c <- s ]
