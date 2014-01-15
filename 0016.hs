
-- Euler Project Problem 16 (Power digit sum)

module Main where
import Data.Char (digitToInt)

main :: IO ()
main = print answer
  where
    answer = sum $ map digitToInt $ show $ 2^1000

