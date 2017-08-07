{-|
File        : Problem1
Description : Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of
these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.

Solution:
The sum of multiples of 3
sum = 3 * 1 + 3 * 2 + ...... + 3 * ((n-1) / 3)
    = 3 * ( 1 + 2 + 3 + ..... + (n-1) / 3 )

sum of numbers from 1 to m:
sum = 1 + 2 + 3 + ..... + (m - 3) + (m - 2) + (m - 1) + m
    = (1 + (m - 1)) + (2 + (m - 2)) + ...... + m
    = m + m + m + m ....... + m
    = (m * (m + 1)) / 2

substitute (n-1)/3, then the sum of multiples of 3:
sum = 3 * ((n-1)/3) * ((n-1)/3 + 1) / 2
  
In general, the sum of multiples of k smaller than n:
sum = k * ((n-1)/k) * (((n-1)/k) + 1)) / 2

-}

import Control.Monad (replicateM_)

main :: IO ()
main = do
  t_temp <- getLine
  let t = read t_temp :: Int
  replicateM_ t ( readLn >>= print . multiplesum )

multiplesum :: Int -> Int
multiplesum n = sum' n 3 + sum' n 5 - sum' n 15
  where sum' n k = (k * (factor n k) * ((factor n k) + 1)) `div` 2;
        factor n k = (n - 1) `div` k
