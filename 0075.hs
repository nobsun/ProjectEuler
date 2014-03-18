
-- Euler Project Problem 75 (Singular integer right triangles)

module Main where

import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy,sortBy,transpose)
import Data.Ord (comparing)
import Data.Tree (Tree(..))

main :: IO ()
main = print answer

answer :: Int
answer = length [ () | l <- pythagoras 1500000 [3,4,5], length l == 1 ]

pythagoras :: Int -> [Int] -> [[([Int], Int)]]
pythagoras lim = groupBy ((==) `on` snd) . sortBy (comparing snd) . concatMap f  . toList . ptree lim
  where f xs = takeWhile ((lim>=) . snd) $ iterate (add xs) xs

add :: ([Int],Int) -> ([Int],Int) -> ([Int],Int)
add (xs,m) (ys,n) = (zipWith (+) xs ys, m+n)

ptree :: Int -> [Int] -> Tree ([Int], Int)
ptree lim v = Node (v,sum v) cs
  where
    cs = map (ptree lim) $ filter ((lim>=) . sum) $ next $ mat v

next :: [[Int]] -> [[Int]]
next m =  transpose (map (`prod` m) pmat)

prod :: [Int] -> [[Int]] -> [Int]
prod v = map (sum . zipWith (*) v)

pmat :: [[Int]]
pmat = [[-1,-2,2],[-2,-1,2],[-2,-2,3]]

mat :: [Int] -> [[Int]]
mat [x,y,z] = [[-x,y,z],[x,-y,z],[-x,-y,z]]
