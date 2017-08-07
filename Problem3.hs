{-|
File        : Problem3
Description : Largest prime

What is the largest prime factor of a given number N?
              
--}

import Control.Monad (replicateM_)
import Data.List

--main :: IO ()
main = do
  t_temp <- getLine
  let t = read t_temp :: Int
  replicateM_ t ( readLn >>= print . last . primeFactors )


-- | with sieve of Eratosthenes
eratosthenes :: Integer -> [Integer]
eratosthenes m = sieve [2..m]
    where sieve [] = []
          sieve (x:xs) = x : sieve (xs \\ [x, x+x..m])

-- | with Turner sieve
turner :: [Integer]
turner = sieve [2..]
    where
      sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

-- | with Postponed Filters Sieve
postponed :: [Integer]
postponed = 2 : 3 : sieve (tail postponed) [5,7..]
  where
    sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where (h, ~(_:t)) = span (< p*p) xs

primes :: [Integer]
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor n (p:ps)
      | p * p > n      = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
