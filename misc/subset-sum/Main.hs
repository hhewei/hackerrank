{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (sort)
import Data.Vector as V (fromList, (!))
import Control.Monad (forM_)

binarySearch :: (Int -> Bool) -> Int -> Int
binarySearch p m =
  loop 0 m
  where
    loop lower upper | lower >= upper && p lower = lower
    loop lower upper | lower >= upper = -1
    loop lower upper =
      let mid = (lower + upper) `div` 2
          (l, u) = if p mid then (lower, mid) else (mid+1, upper) in
      loop l u

main :: IO ()
main = do
  len <- readLn
  (ns :: [Integer]) <- (reverse . sort . map read . words) `fmap` getLine
  let ps = V.fromList$ scanl (+) 0 ns
  nCase <- readLn
  forM_ [1..nCase] $ \iCase -> do
    target <- readLn
    let
      pred a | a < 0 = False
      pred c = target <= (ps!c)
      ans = binarySearch pred len
    print ans
