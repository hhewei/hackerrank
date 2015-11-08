{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (replicateM_)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    n :: Int <- readLn
    print$ 3*n*(n+1)`div`2 - 2*n
