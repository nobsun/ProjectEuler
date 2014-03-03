
-- Euler Project Problem 63 (Powerful digit counts)

module Main where

main :: IO ()
main = print answer

answer :: Int
answer = length $ concat $ takeWhile (not . null) 
       $ zipWith digits [1..] $ map (flip map [1..] . flip (^)) [1..]

digits :: Int -> [Integer] -> [Integer]
digits m (n:ns)
  | s <  m    = digits m ns
  | s == m    = n : digits m ns
  | otherwise = []
    where s = length (show n)