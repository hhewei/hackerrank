module Main where

import Data.IntMap (IntMap, intersectionWith, toList, empty, unionWith, fromList)

primes :: [Int]
primes = filter isPrime [2..10000] where
  isPrime n = all (\d -> n`mod`d /= 0) $ takeWhile (\x -> x*x <= n) [2..]
  
factorization :: Int -> IntMap Int
factorization n = fromList$ f primes n where
  d :: Int -> Int -> (Int, Int)
  d x p | x `mod` p == 0 = let (k, r) = d (x`div`p) p in (k+1, r)
        | otherwise = (0, x)
  f :: [Int] -> Int -> [(Int, Int)]
  f _ 1 = []
  f (p:ps) x | x `mod` p == 0 = let (k, r) = d x p in (p, k) : f ps r
             | otherwise = f ps x
  f [] _ = undefined

factors :: [Int] -> IntMap Int
factors fs = foldl g empty $ map factorization fs where
  g :: IntMap Int -> IntMap Int -> IntMap Int
  g p a = unionWith (+) p a

succint :: IntMap Int -> Integer
succint fs = foldl (\p f -> (p*f)`mod`1000000007) 1 fs' where
  fs' = concat . map (\(p,n) -> replicate n (fromIntegral p)) $ toList fs

main :: IO ()
main = do
  _ <- getLine
  ns <- (map read . words) `fmap` getLine
  _ <- getLine
  ms <- (map read . words) `fmap` getLine
  let fs1 = factors ns
      fs2 = factors ms
      cfs = intersectionWith min fs1 fs2
      ans = succint cfs
  print ans
