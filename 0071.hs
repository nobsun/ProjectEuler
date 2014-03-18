
-- Euler Project Problem 71 (Ordered fractions)

module Main where

import Data.Ratio

main :: IO ()
main = print answer

answer :: Integer
answer = numerator (mediant 1000000)

mediant :: Integer -> Rational
mediant 8 = 2 % 5
mediant m = if m == d' then n' % d' else f'
  where
    f' = mediant (m-1)
    n  = numerator f'
    n' = n + 3
    d  = denominator f'
    d' = d + 7
