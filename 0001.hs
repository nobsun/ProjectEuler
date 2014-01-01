
-- Euler Project Problem 1 (Multiples of 3 and 5)

module Main where

nmuls :: Integral n => n -> n -> n
nmuls = div . pred

summation :: Integral n => (n,n,n) -> n
summation (a,d,n)  =  (n * (a+a+(n-1)*d)) `div` 2

maxb :: Int
maxb =  1000

sum3s, sum5s, sum15s :: Int
sum3s  = summation (3 , 3,nmuls maxb  3)
sum5s  = summation (5 , 5,nmuls maxb  5)
sum15s = summation (15,15,nmuls maxb 15)

main :: IO ()
main = print answer
  where  answer = sum3s + sum5s - sum15s
