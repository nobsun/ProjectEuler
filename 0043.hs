
-- Euler Project Problem 43 (Sub-string divibility)

module Main where

import Data.List (tails,permutations)

main :: IO ()
main = print answer
  where
    answer = sum $ map read $ filter (divisable . subStr) $ permutations "0123456789"

divisable :: [Int] -> Bool
divisable =  and . zipWith divid [2,3,5,7,11,13,17]

divid :: Int -> Int -> Bool
divid d n = n `mod` d == 0

subStr :: String -> [Int]
subStr s = [ read n | n <- take 7 $ map (take 3) $ tail $ tails s ]
