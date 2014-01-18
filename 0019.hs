
-- Euler Project Problem 19 (Counting Sundays)

module Main where

import Data.List (mapAccumL)

main :: IO ()
main = print answer
  where
    answer = length $ filter (0==) $ snd $ mapAccumL (\ a n -> (drop n a, head a)) daysofweek $ daysofmonth

daysofweek :: [Int]
daysofweek = drop (1+365) $ cycle [0..6]

nonleap :: [Int]
nonleap =  [31,28,31,30,31,30,31,31,30,31,30,31]
leap :: [Int]
leap =  [31,29,31,30,31,30,31,31,30,31,30,31]

daysofmonth :: [Int]
daysofmonth = concat $ take 100 $ cycle [nonleap,nonleap,nonleap,leap]
