
-- Euler Project Problem 59 (XOR decryption)

module Main where

import Data.Bits (xor)
import Data.Char (ord,chr,isLetter)
import Data.List (maximumBy)
import Data.List.Split (splitOn) -- cabal install split
import Data.Ord (comparing)

main :: IO ()
main = print . answer =<< readFile "cipher1.txt"

answer :: String -> Int
answer = sum . decrypt . map read . splitOn ","

decrypt :: [Int] -> [Int]
decrypt cs = maximumBy (comparing (length . filter isLetter . map chr))
             [ zipWith xor key cs 
             | key <- [ cycle (map ord [x,y,z]) 
                      | x <- ['a' .. 'z']
                      , y <- ['a' .. 'z']
                      , z <- ['a' .. 'z']]]

