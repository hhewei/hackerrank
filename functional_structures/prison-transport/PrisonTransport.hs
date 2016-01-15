module Main where

import Prelude hiding (lookup)
import Control.Monad (replicateM)
import Data.List (sort, group, foldl')
import Data.IntMap (IntMap, fromList, lookup, insert)

type UnionSet = IntMap Int

main :: IO ()
main = do
  n <- readLn
  m <- readLn
  links <- replicateM m readPair
  let us = fromList [(i,i) | i <- [1..n]]
      us' = foldl' step us links
      us'' = foldl' (\p f -> f p) us' [compress i | i <- [1..n]]
      xs = [last$ repr us'' i | i <- [1..n]]
      ss = sort xs
      gs = group ss
      total = sum$ map (cost . length) gs
  -- print$ size us
  -- print$ size us'
  -- print$ length xs
  -- print$ length ss
  -- print$ length gs
  print total
  where
    readPair :: IO (Int, Int)
    readPair = do
      [i, j] <- words `fmap` getLine
      return (read i, read j)

    step :: UnionSet -> (Int, Int) -> UnionSet
    step us (i,j) = let xs = repr us i in
      foldl' (\p x -> insert x j p) us xs

    repr us x = let Just y = lookup x us in
      if y == x then [x] else x : repr us y

    cost :: Int -> Int
    cost k = head$ dropWhile (\x -> x*x < k) [1..]

    compress :: Int -> UnionSet -> UnionSet
    compress i us = let (x:xs) = reverse$ repr us i in
      foldl' (\p y -> insert y x p) us xs
