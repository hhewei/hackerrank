module Main where

import Data.List (intersperse, sortBy)
import Data.IntMap (IntMap, empty, toList, insertWith)
import Control.Monad (replicateM_)

filter_elements :: [Int] -> Int -> [Int]
filter_elements as k =
  let m = toList$ f empty 0 as
      uptok = filter (\(_,(_,n)) -> n >= k) m
      sorted = sortBy (\(_,(o1,_)) (_,(o2,_)) -> o1 `compare` o2) uptok in
  map fst sorted
  where
    f :: IntMap (Int,Int) -> Int -> [Int] -> IntMap (Int, Int)
    f p _ []= p
    f p i (a:rest) = f (insertWith (\_ (o,n) -> (o,n+1)) a (i,1) p) (i+1) rest

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [_, k] <- (map read . words) `fmap` getLine
    as <- (map read . words) `fmap` getLine
    putStrLn$ case (filter_elements as k) of
     [] -> "-1"
     rs -> concat . intersperse " " $ map show rs
