module Main where

import Data.Bits

newtype Board = Board {
  bits :: Integer
  }

sqoc :: Int -> Integer
sqoc n = sqoc' initial 0 where
  initial = Board 0
  sqoc' b i
    | i == n = 1
    | otherwise = sum [sqoc' (place b (i,j) n) (i+1) | j <- [0..n-1], can b (i,j) n]

can :: Board -> (Int, Int) -> Int -> Bool
can (Board s) (i,j) n = not$ s`testBit`(i*n+j)

place :: Board -> (Int, Int) -> Int -> Board
place (Board s) (i,j) n =
  Board$ foldl setBit s [x*n+y | x <- [i..n-1], y <- [0..n-1], not$ s`testBit`(x*n+y), (x,y) `conflict` (i,j)]

conflict :: (Int, Int) -> (Int, Int) -> Bool
conflict (x,y) (i,j)
  | x == i || y == j = True
  | dxi == dyj = True
  | dxi + dyj <= 3 = True
  | otherwise = False
  where dxi = abs(x-i)
        dyj = abs(y-j)

main :: IO ()
main = do
  n <- readLn
  print$ sqoc n
