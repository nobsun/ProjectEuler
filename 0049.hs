
-- Euler Project Problem 49 (Prime permutations)

module Main where

import Data.List
import Math.NumberTheory.Primes -- cabal install arithmoi

main :: IO ()
main = putStrLn answer

answer :: String
answer = head $ filter ("148748178147"/=) [ concatMap show [p,q,r]
         | p <- range
         , q <- dropWhile (p >=) range
         , sort (show p) == sort (show q)
         , let r = 2*q - p
         , sort (show p) == sort (show r)
         , isPrime r
         ]

range :: [Integer]
range = takeWhile (10000>) $ dropWhile (1001>) primes

