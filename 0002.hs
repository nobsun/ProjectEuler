
-- Euler Project Problem (Even Fibonacci Numbers)

module Main where

sum_efib :: (Int,Int) -> Int
sum_efib (a,b) 
  | b > 4000000 = (a + b - 2) `div` 4
  | otherwise   = sum_efib (b,a+4*b)

main :: IO ()
main = print answer
  where  answer = sum_efib (0,2)

{-
f 0     = 0
f 1     = 1
f (n+2) = f (n+1) + f n 

d n = f (3*n) -- even-valued

d 0     = 0
d 1     = 2
d (n+2) = 4 * d (n+1) + d n

s 0      = 0
s 1      = 2
s (n+2)  =  (d (n+3) + d (n+2) - 2) `div` 4

    s (n+2) - (d 1 + d 0)  =  4 * s (n+1) - 4 * d 0 + s n
=>  s (n+2)                =  4 * s (n+1) + s n + 2
=>  s (n+2)                =  4 * s (n+2) - 4 * d (n+2) + s (n+2) - d (n+2) - d (n+1) + 2
=>  4 * s (n+2)            =  4 * d (n+2) + d (n+1) + d (n+2) - 2
=>  4 * s (n+2)            =  d (n+3) + d (n+2) - 2
=>  s (n+2)                =  (d (n+3) + d (n+2) - 2) `div` 4
-}
