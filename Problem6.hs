{-|
File        : Problem6
Description : Sum square difference

--}

import Control.Monad (replicateM_)
import Data.List

main::IO()
main = do
  t_temp <- getLine
  let t = read t_temp :: Int
  replicateM_ t (readLn >>= print . (\x -> squareOfSum x - sumOfSquares x))

sumOfSquares :: Integer -> Integer
sumOfSquares n = (sum . map (^2)) [1..n]

squareOfSum :: Integer -> Integer
squareOfSum n = sum [1..n] ^ 2
