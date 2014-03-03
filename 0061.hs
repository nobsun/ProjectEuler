
-- Euler Project Problem 61 (Cyclical figurate numbers)

module Main where

import Data.List -- (permutations)

main :: IO ()
main = print $ sum $ head $ concatMap answer 
             $ map (octagonals:) $ permutations 
             $ [triangles,squares,pentagonals,hexagonals,heptagonals]

answer :: [[Integer]] -> [[Integer]]
answer [as,bs,cs,ds,es,fs] 
  = [[a,b,c,d,e,f]
    | a <- as
    , b <- filter (a `followedBy`) bs
    , c <- filter (b `followedBy`) cs
    , d <- filter (c `followedBy`) ds
    , e <- filter (d `followedBy`) es
    , f <- filter (e `followedBy`) fs
    , f `followedBy` a
    ]

followedBy :: Integer -> (Integer -> Bool)
followedBy m n = mod m 100 == div n 100

triangles :: [Integer]
triangles = digits 4 $ map triangle [1..]
triangle :: Integer -> Integer
triangle n = (n*(n+1))`div`2

squares   :: [Integer]
squares   = digits 4 $ map square [1..]
square :: Integer -> Integer
square n =  n*n

pentagonals :: [Integer]
pentagonals = digits 4 $ map pentagon [1..]
pentagon :: Integer -> Integer
pentagon n = (n*(3*n-1))`div`2

hexagonals :: [Integer]
hexagonals = digits 4 $ map hexagon [1..]
hexagon :: Integer -> Integer
hexagon n = n*(2*n-1)

heptagonals :: [Integer]
heptagonals = digits 4 $ map heptagon [1..]
heptagon :: Integer -> Integer
heptagon n = (n*(5*n-3))`div`2

octagonals :: [Integer]
octagonals = digits 4 $ map octagon [1..]
octagon :: Integer -> Integer
octagon n = n*(3*n-2)

digits ::  Int -> [Integer] -> [Integer]
digits m = takeWhile (10^m >) . dropWhile (10^pred m >)
