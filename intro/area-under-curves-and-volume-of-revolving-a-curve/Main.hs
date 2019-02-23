module Main where

import Text.Printf (printf)

type Polynomial = [(Int, Int)]

resolution :: Int
resolution = 1000

width :: Double
width = (fromIntegral 1) / (fromIntegral resolution)

sub :: Int -> Int -> [Double]
sub lower upper = [ fromIntegral lower + width * (fromIntegral i) | i <- [1..n] ]
  where
    diff = upper - lower
    n = resolution * diff

at :: Polynomial -> Double -> Double
poly `at` x = sum$ map (\(a,b) -> (fromIntegral a)*x^b) poly

area :: Polynomial -> Int -> Int -> Double
area poly lower upper = (width * ) . sum $ map f (sub lower upper)
  where f x = poly `at` x

volume :: Polynomial -> Int -> Int -> Double
volume poly lower upper = (pi * width * ) . sum $ map f (sub lower upper)
  where f x = let r = poly `at` x in r * r

readInts :: IO [Int]
readInts = (map read . words) `fmap` getLine

main :: IO ()
main = do
  as <- readInts
  bs <- readInts
  [lower, upper] <- readInts
  let poly = zip as bs
  printf "%f\n" $ area poly lower upper
  printf "%f\n" $ volume poly lower upper
