
-- Euler Project Problem 78 (Coin partitions)

{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative (Applicative,pure,(<$>),(<*>))
import Data.Function.YaMemo (emptyMemoTable,memo') -- cabal install yamemo
import Data.List (mapAccumL,findIndex)
import Data.Map (Map)

main :: IO ()
main = print answer

answer :: Int
answer = maybe undefined succ$findIndex((0==).(`mod`10^6))$snd$mapAccumL p emptyMemoTable [1..]

pF :: (Integral b, Num a) => (b -> a) -> b -> a
pF _ 0 = 1
pF _ 1 = 1
pF f n
 = sum [(-1)^(k-1)*f(n-k')|(k,k')<-takeWhile((n>=).snd)[(k,k')|k<-[1..],let k'=(k*(3*k-1))`div`2]]
 + sum [(-1)^(k-1)*f(n-k')|(k,k')<-takeWhile((n>=).snd)[(k,k')|k<-[1..],let k'=(k*(3*k+1))`div`2]]

instance (Applicative f, Num a) => Num (f a) where
  (+) = (<*>) . ((+) <$>)
  (-) = (<*>) . ((-) <$>)
  (*) = (<*>) . ((*) <$>)
  negate = (negate <$>)
  abs    = (abs <$>)
  signum = (signum <$>)
  fromInteger = pure . fromInteger

p :: Map Int Integer -> Int -> (Map Int Integer, Integer)
p = memo' pF
