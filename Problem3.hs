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

primes :: [Integer]
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor n (p:ps)
      | p * p > n      = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
