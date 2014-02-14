
-- Euler Project Problem 44 (Pentagon numbers)

module Main where

main :: IO ()
main = print answer
  where
    answer = pentagon

pentagons :: [Integer]
pentagons = map penta [1..]

penta :: Integer -> Integer
penta n = (n * (3*n - 1)) `div` 2

isPentagon :: Integer -> Bool
isPentagon = member pentagons
  where
    member (p:ps) n
      | p  <  n  =  member ps n
      | p  >  n  =  False
      | p  == n  =  True
      
pentagon :: Integer
pentagon = iter 1 2
  where
    iter 0 k = iter k (succ k)
    iter m k = if isPentagon d && isPentagon s then d
               else iter (pred m) k
               where
                 pm = penta m 
                 pk = penta k
                 s  = pm + pk
                 d  = pk - pm  
