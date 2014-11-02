module Main where

import Data.ByteString.Char8

newtype Board = Board {
  squares :: ByteString
  }

sqoc :: Int -> Integer
sqoc n = sqoc' initial 0 where
  initial = Board$ pack [' ' | _ <- [1..n*n]]
  sqoc' b i
    | i == n = 1
    | otherwise = sum [sqoc' (place b (i,j) n) (i+1) | j <- [0..n-1], can b (i,j) n]

can :: Board -> (Int, Int) -> Int -> Bool
can (Board s) (i,j) n = s`index`(i*n+j) == ' ' && [] == ['-'| x <- [0..i], y <- [0..j], s`index`(x*n+y) == 'q', (x,y) `conflict` (i,j)]

place :: Board -> (Int, Int) -> Int -> Board
place (Board s) (i,j) n =
  Board$ pack [ if (x,y) == (i,j) then 'q'
          else if (x,y) `conflict` (i,j) then '-' else s`index`(x*n+y)
        | x <- [0..n-1], y <- [0..n-1]]

conflict :: (Int, Int) -> (Int, Int) -> Bool
conflict (x,y) (i,j) | x == i || y == j = True
                     | abs(x-i) == abs(y-j) = True
                     | abs(x-i) + abs(y-j) <= 3 = True
                     | otherwise = False

main :: IO ()
main = do
  n <- readLn
  print$ sqoc n
