
-- Euler Project Problem 32 (Pandigital products)

module Main where

import Data.List (group,sort,permutations)

main :: IO ()
main = print answer
  where
    answer = sum $ map head $ group $ sort [ z | b <- base, t@(_,_,z) <- triple b, check t ]

triple :: String -> [(Int, Int, Int)]
triple xs = [splits 1 4 xs, splits 2 3 xs]

splits :: Int -> Int -> String -> (Int,Int,Int)
splits m n as = case splitAt m as of
  (bs,cs) -> case splitAt n cs of
    (ds,es) -> (read bs,read ds,read es)

check :: (Int, Int, Int) -> Bool
check (a,b,c) = a*b == c

base :: [String]
base = permutations "123456789"
