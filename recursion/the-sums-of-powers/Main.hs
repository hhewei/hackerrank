module Main where

sop :: Int -> Int -> Integer
sop x n = dp x !! x where
  dp 1 = 1 : 1 : [0 | _ <- [2..x]]
  dp i = trans$ dp (i-1) where
    ipn = (fromIntegral i)^n :: Integer
    trans t = [f(k) | k <- [0..x]] where
      f k | fromIntegral k < ipn = t!!k
          | otherwise = t!!k + t!!(k - i^n)

main :: IO ()
main = do
  x <- readLn
  n <- readLn
  print$ sop x n
