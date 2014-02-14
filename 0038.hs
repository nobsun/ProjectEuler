
-- Euler Project Problem 38 (Pandigital multiples)

module Main where

import Data.List (sort)
import Data.Maybe (mapMaybe)

main :: IO ()
main = print answer
  where
    answer = maximum $ mapMaybe prods [1..9876]

pandigit :: String -> Bool
pandigit s = 9 == length s && "123456789" == sort s

prods :: Int -> Maybe Int
prods n = if pandigit m then Just (read m) else Nothing
  where
    m = head $ dropWhile ((9>) . length) 
             $ scanl (\ s x -> s++show (n*x)) "" [1..]
