
-- Euler Project Problem 24 (Lexicographic permutations)

module Main where

import Data.List (mapAccumL)

main :: IO ()
main = putStrLn answer
  where
    answer = concatMap show $ snd $ mapAccumL f (999999,[0..9]) fac10
    f (r0,ns) x = case divMod r0 x of
       (q',r) -> ((r, remove q' ns), ns !! q')

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

remove :: Int -> [Int] -> [Int]
remove n ns = case splitAt n ns of (xs,_:ys) -> xs ++ ys

fac10 :: [Int]
fac10 = reverse $ scanl (*) 1 [1..9]
