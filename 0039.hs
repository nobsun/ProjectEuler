
-- Euler Project Problem 39 (Integer right triangles)

module Main where

import Data.List (maximumBy,sortBy,groupBy,transpose)
import Data.Ord (comparing)

import Data.Tree (Tree(..))
import Data.Function (on)
import Data.Foldable (toList)

main :: IO ()
main = print answer
  where
    answer = snd . head . maximumBy (comparing length) . pythagoras 1000 $ [3,4,5]

pythagoras :: Int -> [Int] -> [[([Int], Int)]]
pythagoras lim = groupBy ((==) `on` snd) . sortBy (comparing snd) . concatMap f  . toList . ptree lim
  where f xs = takeWhile ((lim>=) . snd) $ iterate (add xs) xs

add :: ([Int],Int) -> ([Int],Int) -> ([Int],Int)
add (xs,m) (ys,n) = (zipWith (+) xs ys, m+n)

pmat :: [[Int]]
pmat = [[-1,-2,2],[-2,-1,2],[-2,-2,3]]

mat :: [Int] -> [[Int]]
mat [x,y,z] = [[-x,y,z],[x,-y,z],[-x,-y,z]]

prod :: [Int] -> [[Int]] -> [Int]
prod v = map (sum . zipWith (*) v)

next :: [[Int]] -> [[Int]]
next m =  transpose (map (`prod` m) pmat)

ptree :: Int -> [Int] -> Tree ([Int], Int)
ptree lim v = Node (v,sum v) cs
  where
    cs = map (ptree lim) $ filter ((lim>=) . sum) $ next $ mat v
