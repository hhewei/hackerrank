module Main where

import Control.Monad (forM, forM_)
import Data.List (intersperse)
import Data.Map (Map, findWithDefault, fromList)

type Matrix = Map (Int,Int) Int

data Dir = East | South | West | North
         deriving Eq

main :: IO ()
main = do
  (m : n : r : _) <- readN 3
  dat <- forM [1..m] $ const (readN n)
  let matrix = fromList$ zip [(i,j) | i <- [1..m], j <- [1..n]] (concat dat)
      matrix' = rotate m n r matrix
      out = [[findWithDefault undefined (i,j) matrix' | j <- [1..n]] | i <- [1..m]]
  forM_ out $ \row -> do
    putStrLn$ concat $ intersperse " " $ map show row
  where
    readN :: Int -> IO [Int]
    readN n = do
      ws <- words `fmap` getLine
      let ns = map read ws
      return ns

rotate :: Int -> Int -> Int -> Matrix -> Matrix
rotate m n r matrix = fromList [((i,j), findWithDefault undefined (i',j') matrix) | i <- [1..m], j <- [1..n], let (i',j') = translate (i,j) ] where
  translate (i,j) = inc (i,j) (r `mod` len (i,j))
  len (i,j) = let full = 2 * (m+n) - 4 in
    case dir (i,j) of
      East -> full - 8 * (i-1)
      South -> full - 8 * (n-j)
      West -> full - 8 * (m-i)
      North -> full - 8 * (j-1)
  inc (i,j) 0 = (i,j)
  inc (i,j) c | dir (i,j) == East  = inc (i,j+1) (c-1)
  inc (i,j) c | dir (i,j) == South = inc (i+1,j) (c-1)
  inc (i,j) c | dir (i,j) == West  = inc (i,j-1) (c-1)
  inc (i,j) c | dir (i,j) == North = inc (i-1,j) (c-1)
  dir (i,j) | i <= j && i <= (m`div`2) && (i+j) < (1+n) = East
  dir (i,j) | (i+j) <= (m+1) && j <= (n`div`2) && j < i = North
  dir (i,j) | (i+j) >= (n+1) && j > ((n+1)`div`2) && (i-j) < (m-n) = South
  dir (i,j) = West
