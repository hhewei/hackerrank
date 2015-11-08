{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (replicateM_)
import Data.Array.Unboxed (listArray, (!), UArray)

main :: IO ()
main = do
  t <- readLn
  let m :: Int = 10001
      a :: UArray Int Int = listArray (0,m) . reverse$ fib m
  replicateM_ t $ do
    i <- readLn
    print$ (a!i)
  where
    fib 0 = [0]
    fib 1 = [1,0]
    fib n = let l@(x:y:_) = fib (n-1) in
      ((x+y)`mod`k):l
    k = 100000007
