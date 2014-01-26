
-- Euler Project Problem 26 (Reciprocal cycles)

module Main where

import Control.Applicative ((<*>))
import Data.List (unfoldr,findIndex,maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = print answer
  where
    answer = fst $ maximumBy (comparing snd) $ map f [1..999]
    f = (,) <*> cycleLength . recips
recips :: Int -> [(Int,Int)]
recips n = unfoldr phi 1
  where
    phi m = case divMod m n of
      (0,0)    -> Nothing
      qr@(_,r) -> Just (qr,10*r)

cycleLength :: [(Int,Int)] -> Int
cycleLength = iter []
  where
    iter _  []     = 0
    iter ds (x:xs) = case findIndex (x==) ds of
      Nothing -> iter (x:ds) xs
      Just n  -> succ n
