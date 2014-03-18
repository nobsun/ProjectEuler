
-- Euler Project Problem 66 (Diophantine equation)

module Main where

import Data.List (unfoldr,mapAccumL,maximumBy)
import Data.Ord (comparing)
import Data.Ratio (numerator, denominator)

main :: IO ()
main = print answer

answer :: Integer
answer = fst
       $ maximumBy (comparing (numerator . snd))
       $ map pell
       $ takeWhile (1000>=) nonSquares

cfrac :: Integer -> [Rational]
cfrac d = unfoldr phi (a0,u0,v0)
  where
    dd = toRational d
    sd = toRational (sqrt (fromIntegral d))
    a0 = toRational (floor sd)
    u0 = toRational a0
    v0 = (1 :: Rational)
    phi (a,u,v) = Just (a,(a',u',v'))
        where
          a' = toRational (floor ((sd + u)/v'))
          v' = (dd-u^2)/v
          u' = toRational a'*v'-u

convergence :: Integer -> Int -> Rational
convergence m n = foldr1 ((. recip) . (+)) $ take n $ cfrac m

nonSquares :: [Integer]
nonSquares = concat $ snd $ mapAccumL rmsquare [1..] [1..]
  where
    rmsquare xs x = case break (x^2==) xs of
                      (ys,_:zs) -> (zs,ys)

pell :: Integer -> (Integer,Rational)
pell d = (d,head $ filter p $ map (convergence d) [1..])
  where
    p r = (numerator r)^2 - d * (denominator r)^2 == 1
