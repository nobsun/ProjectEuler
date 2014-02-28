
-- Euler Project Problem 60 (Prime pair sets)

module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Math.NumberTheory.Primes (primes,isPrime)

main :: IO ()
main = print answer

answer :: Integer
answer = sum $ minprimes $ gen5 range

check :: Integer -> Integer -> Bool
check m n = isPrime (read (show m ++ show n)) && isPrime (read (show n ++ show m))

range :: [[Integer]]
range = take 5 (iterate tail (tail primes))

gen5 :: [[Integer]] -> [[Integer]]
gen5 [aas,bbs,ccs,dds,e:es]
  = (if null pps then id else (minimumBy (comparing sum) pps :))
    (gen5 [aas,bbs,ccs,dds,es])
    where
      pps = gen [aas,bbs,ccs,dds] e

gen :: [[Integer]] -> Integer -> [[Integer]]
gen [as,bs,cs,ds] e
  = [[a,b,c,d,e]
    | let as' = takeWhile (e>) as
    , a <- as'
    , check a e
    , let bs' = takeWhile (e>) (dropWhile (a>=) bs)
    , b <- bs'
    , check b e
    , check a b
    , let cs' = takeWhile (e>) (dropWhile (b>=) cs)
    , c <- cs'
    , check c e
    , check a c
    , check b c
    , let ds' = takeWhile (e>) (dropWhile (c>=) ds)
    , d <- ds'
    , check d e
    , check a d
    , check b d
    , check c d
    ]

minprimes :: [[Integer]] -> [Integer]
minprimes (x:xs) = iter x xs
  where
    iter a (b:bs)
      | sx < last b = a
      | sx > sy     = iter b bs
      | otherwise   = iter a bs
           where 
             sx = sum a
             sy = sum b
