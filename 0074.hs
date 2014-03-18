
-- Euler Project Problem 74 (Digit factorial chains)

module Main where

import Data.Char

main :: IO ()
main = print answer

answer :: Int
answer = length $ filter ((60 ==) . count . chain) [1..999999]

chain :: Int -> [Int]
chain n = iterate sumfact n

sumfact :: Int -> Int
sumfact = sum . map (fact . digitToInt) . show

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

count :: [Int] -> Int
count (x:xs) = count' 1 [x] xs

count' :: Int -> [Int] -> [Int] -> Int
count' c xs (y:ys)
  | y `elem` xs = c
  | otherwise   = count' (succ c) (y:xs) ys
