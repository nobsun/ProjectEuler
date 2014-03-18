
-- Euler Project Problem 65 (convergents of e)

module Main where

import Data.Char (digitToInt)
import Data.Ratio (numerator)

main :: IO ()
main = print answer

answer :: Int
answer = sum $ map digitToInt $ show $ numerator $ convergence 100

e :: [Rational]
e = map toRational $ 2 : concatMap f [1..]
    where
      f k = 1 : (2*k) : [1]

convergence :: Int -> Rational
convergence n = foldr1 ((. recip) . (+)) $ take n e