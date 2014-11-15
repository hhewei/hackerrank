module Main where

import Control.Monad (replicateM_)

commonDivisors :: Int -> Int -> Int
commonDivisors m l = product$ map (succ . snd) (decompose (gcd m l))

decompose :: Int -> [(Int, Int)]
decompose n = d' [] primes n where
  d' _ [] _ = undefined
  d' fs _ 1 = fs
  d' fs (p:ps) i
    | x > 0 = d' ((p,x):fs) ps i'
    | otherwise = d' fs ps i
    where
      (i', x) = i `divs` p
      divs a b
        | a `mod` b == 0 = let (r, k) = divs (a `div` b) b in (r, k+1)
        | otherwise = (a, 0)

primes :: [Int]
primes = 2 : [c | c <- [3..], prime c] where
  prime c = not . any (`divides`c) $ takeWhile (\x -> x*x <= c) primes
  a `divides` b = b `mod` a == 0

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    [m, l] <- words `fmap` getLine
    print$ commonDivisors (read m) (read l)
