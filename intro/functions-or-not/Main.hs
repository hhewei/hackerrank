module Main where

import Data.List (nub)
import Control.Monad (replicateM, replicateM_)

validate :: [(Int, Int)] -> Bool
validate points =
  let xs = map fst points
  in length xs == length (nub xs)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    n <- readLn
    pairs <- replicateM n $ do
      line <- getLine
      let [x, y] = map read $ words line
      return (x, y)
    if validate pairs
      then putStrLn "YES"
      else putStrLn "NO"
