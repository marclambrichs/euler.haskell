{-|
File        : Problem8
Description : Largest product in a series

--}

import Control.Monad (replicateM_)
import Data.Char
import Data.List

main::IO()
main = do
  t_temp <- getLine
  let t = read t_temp :: Int
  replicateM_ t ( do
      l_temp <- getLine
      let w = words l_temp
      let n = read $ head w :: Int
      let k = read $ w !! 1 :: Int
      num <- getLine
      print (solution n k num))

solution :: Int -> Int -> String -> Int
solution n k num | k > n || n > 1000 || length num /= n = 0
                 | otherwise                            = (maximum . map productString . pick k) num
    
productString :: String -> Int
productString = foldr ((*) . digitToInt) 1

pick :: Int -> String -> [String]
pick _ [] = [[]]
pick k (x:xs) | length xs >= k - 1 = (x:take (k - 1) xs):pick k xs
              | otherwise          = []
