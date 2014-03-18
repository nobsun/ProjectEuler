
-- Euler Project Problem 76 (Counting summations)

module Main where
import Control.Applicative (Applicative,pure,(<$>),(<*>))
import Data.Function.YaMemo (Memo,memo) -- cabal install yamemo
import Data.Map (Map)

main :: IO ()
main = print answer

answer :: Integer
answer = pred $ cc (reverse [1..100], 100)

memoTable  ::  (Memo Map) ([Int], Int) Integer
memoTable  =   undefined

ccF :: (Applicative f) => (([Int], Int) -> f Integer) -> ([Int], Int) -> f Integer
ccF _ (_   , n) | n < 0           = pure 0
ccF _ ([c] , n) | n `mod` c == 0  = pure 1
                | otherwise       = pure 0
ccF f (ccs@(c:cs), n)             = (+) <$> f (cs,n) <*> f (ccs,n - c)

cc :: ([Int], Int) -> Integer
cc = memo memoTable ccF
