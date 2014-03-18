
-- Euler Project Problem 77 (Prime summations)

module Main where

import Math.NumberTheory.Primes (primes)
import Control.Applicative (Applicative,pure,(<$>),(<*>))
import Data.Function.YaMemo (Memo,memo)
import Data.Map (Map)

main :: IO ()
main = print $ head $ filter ((5000<) . answer) [2..]

answer :: Int -> Integer
answer n = pred $ cc (map fromEnum $ reverse $ takeWhile (toEnum n>=) primes, n)

memoTable  ::  (Memo Map) ([Int], Int) Integer
memoTable  =   undefined

ccF :: (Applicative f) => (([Int], Int) -> f Integer) -> ([Int], Int) -> f Integer
ccF _ (_   , n) | n < 0          = pure 0
ccF _ ([c] , n) | n `mod` c == 0 = pure 1
                | otherwise      = pure 0
ccF f (ccs@(c:cs), n)    = (+) <$> f (cs,n) <*> f (ccs,n - c)

cc :: ([Int], Int) -> Integer
cc = memo memoTable ccF
