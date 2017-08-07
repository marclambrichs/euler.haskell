{-|
File        : Problem2
Description : Even Fibonacci numbers

By considering the terms in the Fibonacci sequence whose values do not exceed N,
find the sum of even-valued terms.
              
--}

import Control.Monad (replicateM_)

main :: IO ()
main = do
  t_temp <- getLine
  let t = read t_temp :: Int
  replicateM_ t ( readLn >>= print . fib (1, 1)  0 )

fib :: (Int, Int) -> Int -> Int -> Int
fib (prev1, prev2) acc n | sum' > n               = acc
                         | sum' <= n && even sum' = fib (prev2, sum') (acc + sum') n
                         | otherwise              = fib (prev2, sum') acc n
                              where sum' = prev1 + prev2

