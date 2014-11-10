module Main where

import Data.Bits
import Data.List (nub)
import Data.IntMap (IntMap, fromList, (!))

newtype Board = Board {
  bits :: Integer
  }

sqoc :: Int -> Integer
sqoc n = sqoc' (Board 0) 0 where
  sqoc' b i
    | i == n = 1
    | otherwise = sum [sqoc' (place b (i,j)) (i+1) | j <- [0..n-1], can b (i,j)]
  
  can :: Board -> (Int, Int) -> Bool
  can (Board s) (i,j) = not$ s`testBit`(i*n+j)

  place :: Board -> (Int, Int) -> Board
  place (Board s) (i,j) = Board$ s .|. (patterns!(i*n+j))

  patterns :: IntMap Integer
  patterns = fromList [(i*n+j, pattern i j) | i <- [0..n-1], j <- [0..n-1]] where
    pattern i j = foldl setBit 0 [i'*n+j' | (di,dj) <- offsets, let i' = i+di, let j' = j+dj, on i', on j']
    on x = 0 <= x && x < n
    offsets = nub$ [(i,j) | i <- [-2..2], j <- [-2..2]] ++ concat [[(0,i),(i,0),(i,i),(i,-i)] | i <- [-(n-1)..(n-1)]]

main :: IO ()
main = do
  n <- readLn
  print$ sqoc n
