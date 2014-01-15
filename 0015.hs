
-- Euler Project Problem 15 (Lattice paths)

module Main where

main :: IO ()
main = print answer
  where
    answer = product [21..40] `div` product [1..20] :: Integer
             -- (2n!)/(n!)(n!)
