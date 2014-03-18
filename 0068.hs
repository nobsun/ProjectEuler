
-- Euler Project Problem 68 (Magic 5-gon ring)

{-# LANGUAGE NPlusKPatterns #-}
module Main where

import Control.Applicative ((<*>))
import Control.Arrow (first,second,(***))
import Data.List 

main :: IO ()
main = putStrLn answer

answer :: String
answer = maximum 
       $ filter ((16==) . length) 
       $ map (concatMap (concatMap show)) 
       $ concatMap magic divids

comb :: [a] -> Int -> [([a],[a])]
comb []  _ = []
comb [x] 1 = [([x],[])]
comb xs  0 = [([],xs)]
comb (x:xs) (k+1) = map (first (x:)) (comb xs k) ++ map (second (x:)) (comb xs (k+1))

divids :: [(([Int], [Int]), Int)]
divids = filter ((==0) . (`mod` 5) . snd) $ map ((,) <*> (uncurry (+) . (sum *** (2*) . sum))) $ comb [1..10] 5

magic :: (([Int],[Int]),Int) -> [[[Int]]]
magic ((x:xs,ys),s)
  = filter (all ((sm==) . sum)) [ edges a b | a <- o, b <- i ]
    where sm = s `div` 5
          o  = map (x:) (permutations xs)
          i  = permutations ys

edges :: [Int] -> [Int] -> [[Int]]
edges o i = zipWith (:) o (slices 2 (cycle i))

slices :: Int -> [a] -> [[a]]
slices n = unfoldr phi
  where
    phi [] = Nothing
    phi xs = Just (take n xs, tail xs)