
-- Euler Project Problem 53 (Combinatoric selections)

module Main where

import Data.List (mapAccumR)

main :: IO ()
main = print answer
  where
    answer = length $ filter (1000000<) $ concat $ drop 23 $ take 101 pascal

pascal :: [[Integer]]
pascal = [1] : [f cs | cs <- pascal ]
  where
    f xs = snd $ mapAccumR g xs' xs'
           where xs'        = 0:xs
                 g (y:ys) z = (ys, y+z)

