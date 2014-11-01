module Main where

import Control.Monad (replicateM_)

sop :: String -> String
sop [] = []
sop (a:b:r) = b : a : sop r
sop _ = undefined

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    l <- getLine
    putStrLn $ sop l
