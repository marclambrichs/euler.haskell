{-|
File        : Problem4
Description : Largest palindrome product

--}

import Control.Monad (replicateM_)
import Data.List

main :: IO ()
main = do
  t_temp <- getLine
  let t = read t_temp :: Int
  replicateM_ t ( readLn >>= print . last . sort . products )

products :: Int -> [Int]
products n = [ z | x <- [999, 998 .. 100], y <- [999, 998 .. x], let z = x * y, z < n, let s = show z, reverse s == s ]
