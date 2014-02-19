
-- Euler Project Problem 51 (Prime digit replacements)

module Main where

import Control.Applicative ((<*>))
import Data.List (group,sort)
import Math.NumberTheory.Primes (primes,isPrime) -- cabal install arithmoi

main :: IO ()
main = putStrLn answer

answer :: String
answer = fst $ head $ filter g $ filter (not . null . snd) $ map ((,) <*> f) $ map show $ dropWhile (1000 >) $ primes

f :: String -> [Char]
f = map head . filter ("222">=) . filter ((3 ==) . length) . group . sort . init

g :: (String,[Char]) -> Bool
g (_,[])     = False
g (s,'0':ds) = 7 <= length (filter id (map (h '0' s) ['1'..'9'])) || g (s,ds)
g (s,'1':ds) = 7 <= length (filter id (map (h '1' s) ['2'..'9'])) || g (s,ds)
g (s,'2':ds) = 7 <= length (filter id (map (h '2' s) ['3'..'9'])) || g (s,ds)

h :: Char -> String -> Char -> Bool
h i s c = isPrime (read (map (k i c) s))

k :: Char -> Char -> (Char -> Char)
k i c d | i == d    = c
        | otherwise = d
