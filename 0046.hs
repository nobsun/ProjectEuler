
-- Euler Project Problem 46 (Goldbach's other conjecture)

module Main where

import Math.NumberTheory.Primes  -- cabal install arithmoi

main :: IO ()
main = print answer

answer :: Integer
answer = iter 25
  where
    iter n = if check 1 n then iter (n+2) else n

check :: Integer -> Integer -> Bool
check _ n | isPrime n       = True
check m n | n < m'          = False
          | isPrime (n-m')  = True
          | otherwise       = check (m+1) n
  where
    m' = 2*m^2