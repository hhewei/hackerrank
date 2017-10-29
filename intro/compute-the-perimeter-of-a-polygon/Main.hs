module Main where

import Control.Monad (replicateM)

type Point = (Int, Int)

dist :: (Point, Point) -> Double
dist ((x1,y1) , (x2,y2)) = sqrt . fromIntegral $ dx*dx + dy*dy
  where
    dx = x1 - x2
    dy = y1 - y2

main :: IO ()
main = do
  n <- readLn
  ps <- replicateM n $ do
    l <- getLine
    let [x, y] = map read $ words l
    return (x, y)
  let segments = zip ps . drop 1 $ cycle ps
  print . sum $ map dist segments
