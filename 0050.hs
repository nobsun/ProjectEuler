
-- Euler Project Problem 50 (Consecutive prime sum)

module Main where

import Data.List (find, transpose, tails, inits)
import Data.Maybe (mapMaybe)
import Math.NumberTheory.Primes (primes,isPrime) -- cabal install arithmoi

main :: IO ()
main = print answer

answer :: Integer
answer = last $ mapMaybe (find isPrime) $ map (takeWhile (1000000>)) 
       $ takeWhile ((1000000>) . head) $ map  (map sum)
       $ takeEven $ drop 21 
       $ transpose $ map inits $ tails $ takeWhile (1000000>) primes

takeEven :: [a] -> [a]
takeEven []  = []
takeEven [x] = [x]
takeEven (x:_:zs) = x : takeEven zs
