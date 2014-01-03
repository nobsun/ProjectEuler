
-- Euler Project Problem 4 (Longest palindrome product)

module Main where

main :: IO ()
main = print answer
  where
    answer = maximum [ a | i <- [100..999], j <- [i..999]
                         , let a = i*j, palindrome a ]
    palindrome n = palin (show n)
    palin s = s == reverse s
