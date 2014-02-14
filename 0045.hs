
-- Euler Project Problem 45 (Triangular, pentagonal, and hexagonal)

module Main where

main :: IO ()
main = print $ answer (drop 285 triangles) (drop 165 pentagonals) (drop 143 hexagonals)

answer :: [Integer] -> [Integer] -> [Integer] -> Integer
answer tts pps (h:hs) = case member tts h of
  Right ts -> case member pps h of
    Right _  -> h
    Left ps  -> answer ts ps hs
  Left ts -> answer ts pps hs

triangles :: [Integer]
triangles = map t [1..]
  where
    t n = (n * (n + 1)) `div` 2

pentagonals :: [Integer]
pentagonals = map p [1..]
  where
    p n = (n * (3*n -1)) `div` 2

hexagonals :: [Integer]
hexagonals = map h [1..]
  where
    h n = n * (2*n -1)

member :: [Integer] -> Integer -> Either [Integer] [Integer]
member xxs@(x:xs) n | x  <  n   = member xs n
                    | x  >  n   = Left xxs
                    | otherwise = Right xs




