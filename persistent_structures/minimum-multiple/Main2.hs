{-# LANGUAGE Strict #-}

import Control.Monad (forM_, forM, foldM)
import Data.IntMap (IntMap, fromList)
import Data.Array (Array, listArray, (!))
import Data.Array.IO (IOUArray(..), newArray, readArray, writeArray)

type Table = IOUArray Int Int

primes :: [Int]
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

smallPrimes :: [Int]
smallPrimes = takeWhile (<100) primes

m :: Int
m = length smallPrimes

i2p :: Array Int Int
i2p = listArray (0,m-1) smallPrimes

factors :: Int -> [(Int, Int)]
factors x = loop x 0 []
  where
    loop 1 i p = p
    loop n i p | n `mod` (i2p!i) == 0 = let (r, c) = fact n (i2p!i) in loop r (i+1) ((i,c):p)
    loop n i p = loop n (i+1) p

    fact :: Int -> Int -> (Int, Int)
    fact num den = f num 0
      where f x acc | x `mod` den == 0 = f (x `div` den) (acc+1)
            f x acc = (x, acc)

readInts :: IO [Int]
readInts = do
  toks <- words `fmap` getLine
  return$ map read toks

process :: Int -> Table -> String -> IO ()
process n t (a:_:rest) = do
  let [b, c] = map read $ words rest
  case a of
    'Q' -> query n t b c
    'U' -> update n t b c

query :: Int -> Table -> Int -> Int -> IO ()
query n t l r = do
  fs <- forM [0..(m-1)] $ \i -> do
    --opt <- foldM (\b j -> (readArray t (i*n+j)) >>= pure . max b) 0 [l..r]
    opt <- loop i l
    return$! pow (i2p!i) opt
  print$ prd fs
  where loop :: Int -> Int -> IO Int
        loop i p | p == r = readArray t (i*n+p)
        loop i p = do
          x <- loop i (p+1)
          y <- readArray t (i*n+p)
          return$! max x y

prd :: [Int] -> Int
prd fs = prd' fs 1
  where prd' [] p = p
        prd' (h:t) p = prd' t ((h*p) `mod` modulo)

pow :: Int -> Int -> Int
pow x 0 = 1
pow x n | n `mod` 2 == 0 =
            let a = pow x (n`div`2) in
              (a*a) `mod` modulo
pow x n = (x * pow x (n-1)) `mod` modulo

modulo :: Int
modulo = 1000000000 + 7

update :: Int -> Table -> Int -> Int -> IO ()
update n t idx val= do
  let fs = factors val
  forM_ fs $ \(i,c) -> do
    let loc = i*n + idx
    orig <- readArray t loc
    writeArray t loc (orig + c)

main :: IO ()
main = do
  [n] <- readInts
  table <- newArray (0,(n*m)-1) 0
  nums <- readInts
  forM_ (zip [0..] nums) $ \(i,num) -> do
    update n table i num
  _ <- getLine
  ls <- lines `fmap` getContents
  forM_ ls (process n table)
