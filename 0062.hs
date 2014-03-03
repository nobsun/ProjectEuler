
-- Euler Project Problem 62 (Cubic permutations)

module Main where

import Control.Applicative ((<*>))
import Data.List (mapAccumL,sort,sortBy,groupBy,maximumBy,find)
import Data.Ord (comparing)
import Data.Function (on)

main :: IO ()
main = print answer

cubes :: [[Integer]]
cubes =  snd $ (flip (mapAccumL f) <*> map (^3)) [1..]
  where
    f cs n = swap (span (10^n>) cs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

tuple :: Integer -> (Integer,Integer)
tuple = (,) <*> read . sort . show

answer :: Integer
answer = fst $ head $ minimum $ map sort $ head $ filter (not . null)
       $ map (filter ((5==) . length) . groupBy ((==) `on` snd) . sortBy (comparing snd) . map tuple)
       $ cubes