module Main where

import Data.List

{-
    n!
-----------
r! * (n-r)!

-}
a :: Integer -> Integer -> Integer
a n r = fac n `div` fac r `div` fac (n-r)

fac :: Integer -> Integer
fac n = product [1..n]

main :: IO ()
main = do
  k <- readLn
  mapM_ printRow [[a n r | r <- [0..n]] | n <- [0..k-1]]
  where
    printRow as = putStrLn $ concat $ intersperse " " $ map show as
