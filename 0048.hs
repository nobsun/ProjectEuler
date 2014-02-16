
-- Euler Project Problem 48 (Self powers)

module Main where

main :: IO ()
main = putStrLn answer

answer :: String
answer =  reverse $ take 10 $ reverse $ show $ sum [ n^n | n <- [1..1000]]
