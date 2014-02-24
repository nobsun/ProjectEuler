
-- Euler Project Problem 54 (Poker hands)

module Main where

import Control.Applicative ((<*>))
import Data.Char (ord, digitToInt)
import Data.Function (on)
import Data.List (sortBy,groupBy)
import Data.Ord (comparing)

main :: IO ()
main = answer
  where
    answer = print . count =<< readFile "poker.txt"

count :: String -> Int
count = length . filter id . map (win . splitAt 5 . map conv . words) . lines

type Card = (Int,Int)
type Hand = [Card]

conv :: String -> Card
conv [r,c] = (r',ord c)
  where r' = case r of
               'T' -> 10
               'J' -> 11
               'Q' -> 12
               'K' -> 13
               'A' -> 14
               _   -> digitToInt r

win :: (Hand, Hand) -> Bool
win (h1,h2) = hand h1 > hand h2

hand :: Hand -> (Rank,[Int])
hand = rank . sortBy (flip compare)

data Rank = HC | OP | TP | TK | ST | FL | FH | FK | SF | RF deriving (Eq,Ord,Show)

rank :: Hand -> (Rank,[Int])
rank cs 
  | isFlush cs    = if not $ isStraight cs then (FL, map fst cs)
                    else if 14 == fst (head cs) then (RF, map fst cs)
                         else (SF, map fst cs)
  | isStraight cs = (ST, map fst cs)
  | otherwise     = case groupBy ((==) `on` fst) cs of
       gs@[_,_]      -> case sortBy (flip (comparing length)) gs of
          [xs,ys]       -> if 4 == length xs then (FK, map fst (xs++ys))
                           else (FH, map fst (xs++ys))
       gs@[_,_,_]    -> case sortBy (flip (comparing length)) gs of
          [xs,ys,zs]    -> if 3 == length xs then (TK, map fst (xs++ys++zs))
                           else (TP, map fst (xs++ys++zs))
       gs@[_,_,_,_]  -> (OP, concatMap (map fst) (sortBy (flip (comparing length)) gs))
       _             -> (HC, map fst cs)

isFlush :: Hand -> Bool
isFlush = and . (zipWith ((==) `on` snd) <*> tail)

isStraight :: Hand -> Bool
isStraight cs = (fst (head cs) - 4 == fst (last cs))
                && 
                5 == length (groupBy ((==) `on` fst) cs)
