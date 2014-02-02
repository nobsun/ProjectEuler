
-- Euler Project Problem 31 (Coin sums)

module Main where

import Control.Applicative
import Data.Function.YaMemo (Memo, memo) -- cabal install yamemo
import Data.Map (Map)

main :: IO ()
main = print answer
  where
    answer = cc (coinsGB,200)

coinsGB  ::  [Int]
coinsGB  =   reverse [1,2,5,10,20,50,100,200]

memoTable  ::  (Memo Map) ([Int], Int) Int
memoTable  =   undefined

ccF :: (Monad m, Applicative m) => (([Int], Int) -> m Int) -> ([Int], Int) -> m Int
ccF _ (_   , n) | n < 0  = return 0
ccF _ ([1] , _)          = return 1
ccF f (ccs@(c:cs), n)    = (+) <$> f (cs,n) <*> f (ccs,n - c)

cc :: ([Int], Int) -> Int
cc = memo memoTable ccF
