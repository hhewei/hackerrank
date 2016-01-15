{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Prelude hiding (lookup)
import Control.Monad (foldM)
import Data.Array.IO.Safe (IOUArray, newArray, readArray, writeArray, getBounds)

main :: IO ()
main = do
  n <- readLn
  ws <- words `fmap` getLine
  let hs = take n $ map read ws
      h = maximum hs
  (arr :: IOUArray Int Int) <- newArray (1,h) 0
  (m,_) <- foldM (step arr) (0,0) hs
  print m
  where
    step :: IOUArray Int Int -> (Int,Int) -> Int -> IO (Int,Int)
    step arr (!m,i) h = do
      (_, high) <- getBounds arr
      --putStrLn $ "#" ++ show i
      --print "plus1"
      plus1 arr h
      --print "reset"
      reset arr (h+1) high
      --print "opt"
      m' <- opt arr (h,0)
      return (max m' m, i+1)
      
    plus1 :: IOUArray Int Int -> Int -> IO ()
    plus1 _ 0 = return ()
    plus1 arr i = do
      e <- readArray arr i
      writeArray arr i (e+1)
      plus1 arr (i-1)

    reset :: IOUArray Int Int -> Int -> Int -> IO ()
    reset _ i b | i > b = return ()
    reset arr i b = do
      writeArray arr i 0
      reset arr (i+1) b

    opt :: IOUArray Int Int -> (Int, Int) -> IO Int
    opt _ (0, m) = return m
    opt arr (i, m) = do
      e <- readArray arr i
      let m' = i * e
      opt arr (i-1, max m' m)
