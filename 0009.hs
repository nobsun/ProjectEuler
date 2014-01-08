
-- Euler Project Problem 9 (Special Pythagorean triplet)

module Main where

main :: IO ()
main = print answer
  where
    answer = head [ a*b*c | a <- [1..333]
                          , b <- [a+1 .. (1000-a)`div`2]
                          , let c = 1000 - (a + b)
                          , a*a + b*b == c*c
                  ] 
