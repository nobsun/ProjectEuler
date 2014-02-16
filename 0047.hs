
-- Euler Project Problem 47 (Distinct primes factors)

module Main where

import Math.NumberTheory.Primes  -- cabal install arithmoi

main :: IO ()
main = print answer

answer :: Integer
answer =  iter 647
  where
    iter n = case check n of
      Left m  -> iter m
      Right k -> k

check :: Integer -> Either Integer Integer
check n = if 4 /= length (factorise n) then Left (n+1)
          else if 4 /= length (factorise (n+1)) then Left (n+2)
               else if 4 /= length (factorise (n+2)) then Left (n+3)
                     else if 4 /= length (factorise (n+3)) then Left (n+4)
                          else Right n
