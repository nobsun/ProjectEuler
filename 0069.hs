
-- Euler Project Problem 69 (Totient maximum)

module Main where

import Control.Applicative ((<*>))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Math.NumberTheory.Primes (φ)
                                 
main :: IO ()
main = print answer

answer :: Integer
answer = maximumBy (comparing (((/) . toRational) <*> toRational . φ)) [2..1000000]
