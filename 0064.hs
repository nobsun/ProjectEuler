
-- Euler Project Problem 64 (Odd period square roots)

module Main where

import Data.List (unfoldr,findIndex,mapAccumL)

main :: IO ()
main = print answer

answer :: Int
answer = length $ filter odd $ map (period . cfrac) (takeWhile (10000>=) nonSquares)

cfrac :: Int -> [Int]
cfrac d = unfoldr phi (a0,u0,v0)
  where
    dd = toRational d
    sd = toRational (sqrt (fromIntegral d))
    a0 = floor sd
    u0 = toRational a0
    v0 = (1 :: Rational)
    phi (a,u,v) = Just (a,(a',u',v'))
        where
          a' = floor ((sd + u)/v')
          v' = (dd-u^2)/v
          u' = toRational a'*v'-u

period :: [Int] -> Int
period (x:xs) = maybe 0 (1+) (findIndex (2*x==) xs)

nonSquares :: [Int]
nonSquares = concat $ snd $ mapAccumL rmsquare [1..] [1..]
  where
    rmsquare xs x = case break (x^2==) xs of
                      (ys,_:zs) -> (zs,ys)