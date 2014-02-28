
-- Euler Project Problem 57 (Square root convergents)

module Main where

import Control.Arrow ((&&&))
import Data.Ratio

main :: IO ()
main = print answer
  where
    answer = length $ filter check $ take 1000 $ tail cfracs

cfracs :: [Rational]
cfracs = iterate f 1
  where
    f r = 1 + 1 / (1 + r)

check :: Rational -> Bool
check = uncurry (>) . (length . show . numerator &&& length . show . denominator)
