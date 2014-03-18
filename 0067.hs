
-- Euler Project Problem 67 (Maximum path sum II)

module Main where

main :: IO ()
main = print . answer =<< readFile "triangle.txt"

answer :: String -> Int
answer = maximum . foldl1 (zipWith (+) . extend)
       . map (map toInt . words) . lines

extend :: [Int] -> [Int]
extend xs = zipWith max ([0]++xs) (xs++[0])

toInt :: String -> Int
toInt =  read
