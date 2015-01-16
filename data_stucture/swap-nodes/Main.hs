module Main where

import Control.Monad (replicateM, forM_)
import Data.IntMap (IntMap, fromList, (!))
import Data.List (intersperse)

data Tree =
  Branch Int Tree Tree | Empty

fromMap :: IntMap (Int,Int) -> Tree
fromMap m = f 1 where
  f (-1) = Empty
  f root = let (l, r) = (m!root) in
    Branch root (f l) (f r)

swap :: Tree -> Int -> Tree
swap tree k = rotate tree 1 where
  rotate Empty _ = Empty
  rotate (Branch c l r) d | d `mod` k == 0 = Branch c r' l'
                          | otherwise = Branch c l' r'
    where (l', r') = (rotate l (d+1), rotate r (d+1))

printInOrder :: Tree -> IO ()
printInOrder tree = putStrLn$ concat . intersperse " " . map show $ inOrder tree where
  inOrder Empty = []
  inOrder (Branch c l r) = inOrder l ++ (c : inOrder r)

main :: IO ()
main = do
  n <- readLn
  pairs <- replicateM n $ do
    line <- getLine
    let [l, r] = words line
    return (read l, read r)
  t <- readLn
  ks <- replicateM t readLn
  
  let tree = fromMap . fromList $ zip [1..n] pairs
      trees = tail$ scanl swap tree ks
  forM_ trees printInOrder
